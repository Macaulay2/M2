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

-- Test mapping_polytope/2-B.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,-1}};
raysP = map(QQ^1, QQ^0, 0);
linealityP = map(QQ^1, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1},{-1}};
ineqrhsPd = matrix {{1},{1}};
eqlhsPd = map(QQ^0, QQ^1, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 2)
assert(volume Pd == 2)
LE = reverse {1,2};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test mapping_polytope/1-A.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,-1}};
raysP = map(QQ^1, QQ^0, 0);
linealityP = map(QQ^1, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1},{-1}};
ineqrhsPd = matrix {{1},{1}};
eqlhsPd = map(QQ^0, QQ^1, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 2)
assert(volume Pd == 2)
LE = reverse {1,2};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test mapping_polytope/1-B.poly
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

-- Test mapping_polytope/2.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{-1,1,1,-1,1,-1,1,-1},{2,0,0,2,-2,0,-2,0},{0,-2,0,2,0,2,-2,0}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,0,0},{-1,0,0},{1,1,0},{-1,-1,0},{1,0,1},{-1,0,-1}};
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

-- Test mapping_polytope/2-A.poly
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

-- Test CUBICALITY/1.poly
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

-- Test CUBICALITY/2.poly
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

-- Test GRAPH_CONNECTED/1.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0}};
raysP = map(QQ^1, QQ^0, 0);
linealityP = map(QQ^1, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0}};
ineqrhsPd = matrix {{1}};
eqlhsPd = matrix {{-1}};
eqrhsPd = matrix {{0}};
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

-- Test GRAPH_CONNECTED/2.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{-1,1}};
raysP = map(QQ^1, QQ^0, 0);
linealityP = map(QQ^1, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1},{1}};
ineqrhsPd = matrix {{1},{1}};
eqlhsPd = map(QQ^0, QQ^1, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 2)
assert(volume Pd == 2)
LE = reverse {1,2};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test simplex/7l.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,0,0,1},{0,0,2,2},{0,3,3,3}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,0,1},{-1,0,0},{2,-1,0},{0,3/2,-1}};
ineqrhsPd = matrix {{3},{0},{0},{0}};
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

-- Test simplex/1.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,1,0,0,0},{0,0,1,0,0},{0,0,0,1,0},{0,0,0,0,1}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,1,1,1},{-1,0,0,0},{0,-1,0,0},{0,0,-1,0},{0,0,0,-1}};
ineqrhsPd = matrix {{1},{0},{0},{0},{0}};
eqlhsPd = map(QQ^0, QQ^4, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 1/24)
assert(volume Pd == 1/24)
LE = reverse {1,25/12,35/24,5/12,1/24};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test simplex/6f.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,0,0,0,0,0,-1},{0,1,0,0,0,0,-1},{0,0,1,0,0,0,-1},{0,0,0,1,0,0,-1},{0,0,0,0,1,0,-1},{0,0,0,0,0,1,-1}};
raysP = map(QQ^6, QQ^0, 0);
linealityP = map(QQ^6, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,1,1,1,1,1},{1,1,1,1,1,-6},{1,-6,1,1,1,1},{1,1,-6,1,1,1},{1,1,1,-6,1,1},{1,1,1,1,-6,1},{-6,1,1,1,1,1}};
ineqrhsPd = matrix {{1},{1},{1},{1},{1},{1},{1}};
eqlhsPd = map(QQ^0, QQ^6, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 7/720)
assert(volume Pd == 7/720)
LE = reverse {1,49/20,133/45,49/48,77/144,7/240,7/720};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test simplex/0.poly
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

-- Test triang_boundary/1.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,1,0,1,0,1,0,1},{0,0,1,1,0,0,1,1},{0,0,0,0,1,1,1,1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,0,-1},{0,-1,0},{-1,0,0},{0,0,1},{1,0,0},{0,1,0}};
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

-- Test birkhoff/1.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,0,0,1,0,0,0,0,1,0,0,1,1,0,0,1,0,0,0,0,0,0,0,0},{0,1,0,0,1,0,0,1,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,1},{0,0,1,0,0,1,0,0,0,0,0,0,0,1,0,0,1,0,0,1,0,0,1,0},{0,0,0,0,0,0,1,0,0,1,0,0,0,0,1,0,0,1,1,0,0,1,0,0},{0,1,1,0,0,0,0,0,0,1,1,0,0,1,1,0,0,0,0,0,0,0,0,0},{1,0,0,0,0,1,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,1,0},{0,0,0,1,1,0,0,0,0,0,0,0,1,0,0,0,0,1,1,0,0,0,0,1},{0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,1,1,0,0,0},{0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0},{0,0,1,1,0,0,0,0,1,1,0,0,0,0,0,0,0,0,1,1,0,0,0,0},{1,1,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,1,1,0,0},{0,0,0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,1,1},{0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1},{0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,0,0},{0,0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0},{1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}};
raysP = map(QQ^16, QQ^0, 0);
linealityP = map(QQ^16, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,0,0,0,0,0,0,0,0,0,-1,0,0,0,0,0},{-1,-1,-1,0,-1,-1,-1,0,-1,-1,-1,0,0,0,0,0},{-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{0,0,0,0,0,-1,0,0,0,0,0,0,0,0,0,0},{0,0,1,0,0,0,1,0,0,0,1,0,0,0,0,0},{0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0},{0,0,0,0,0,0,0,0,0,-1,0,0,0,0,0,0},{0,0,0,0,0,0,-1,0,0,0,0,0,0,0,0,0},{0,1,0,0,0,1,0,0,0,1,0,0,0,0,0,0},{0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0},{0,-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{0,0,0,0,-1,0,0,0,0,0,0,0,0,0,0,0},{0,0,0,0,0,0,0,0,-1,0,0,0,0,0,0,0},{0,0,-1,0,0,0,0,0,0,0,0,0,0,0,0,0},{1,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0},{1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0}};
ineqrhsPd = matrix {{0},{-2},{0},{0},{1},{1},{0},{0},{1},{1},{0},{0},{0},{0},{1},{1}};
eqlhsPd = matrix {{-1,-1,-1,-1,0,0,0,0,0,0,0,0,0,0,0,0},{0,0,0,0,-1,-1,-1,-1,0,0,0,0,0,0,0,0},{0,0,0,0,0,0,0,0,-1,-1,-1,-1,0,0,0,0},{-3/4,1/4,1/4,1/4,-3/4,1/4,1/4,1/4,-3/4,1/4,1/4,1/4,-1,0,0,0},{1/13,-9/13,4/13,4/13,1/13,-9/13,4/13,4/13,1/13,-9/13,4/13,4/13,-3/13,-1,0,0},{1/10,1/10,-3/5,2/5,1/10,1/10,-3/5,2/5,1/10,1/10,-3/5,2/5,-3/10,-3/10,-1,0},{1/7,1/7,1/7,-3/7,1/7,1/7,1/7,-3/7,1/7,1/7,1/7,-3/7,-3/7,-3/7,-3/7,-1}};
eqrhsPd = matrix {{-1},{-1},{-1},{-1/4},{-4/13},{-2/5},{-4/7}};
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 11/11340)
assert(volume Pd == 11/11340)
LE = reverse {1,65/18,379/63,35117/5670,43/10,1109/540,2/3,19/135,11/630,11/11340};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test birkhoff/2.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,0,0,0,1,0,1,0,0,0,0,0},{0,0,1,0,0,1,0,0,0,0,1,0},{0,1,0,0,0,0,0,0,1,0,0,1},{0,0,0,1,0,0,0,1,0,1,0,0},{0,1,0,0,0,1,0,1,0,0,0,0},{1,0,0,1,0,0,0,0,0,0,0,1},{0,0,1,0,0,0,1,0,0,1,0,0},{0,0,0,0,1,0,0,0,1,0,1,0},{0,0,1,1,0,0,0,0,1,0,0,0},{0,1,0,0,1,0,0,0,0,1,0,0},{1,0,0,0,0,0,0,1,0,0,1,0},{0,0,0,0,0,1,1,0,0,0,0,1},{0,0,0,0,0,0,0,0,0,1,1,1},{0,0,0,0,0,0,1,1,1,0,0,0},{0,0,0,1,1,1,0,0,0,0,0,0},{1,1,1,0,0,0,0,0,0,0,0,0}};
raysP = map(QQ^16, QQ^0, 0);
linealityP = map(QQ^16, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,-1,-1,0,-1,-1,-1,0,-1,-1,-1,0,0,0,0,0},{-1,-1,-2,0,-1,-2,-1,0,0,-1,-1,0,0,0,0,0},{-1,-1,0,0,-1,-2,-1,0,-2,-1,-1,0,0,0,0,0},{0,0,0,0,0,-1,0,0,0,0,0,0,0,0,0,0},{-1,0,-1,0,-2,-1,-1,0,-1,-1,-2,0,0,0,0,0},{0,1,1,0,-1,-1,0,0,-1,0,-1,0,0,0,0,0},{-1,-2,-1,0,0,-1,-1,0,-1,-1,-2,0,0,0,0,0},{0,-1,-1,0,1,-1,0,0,1,0,-1,0,0,0,0,0},{0,0,0,0,0,0,0,0,0,0,-1,0,0,0,0,0},{-2,-1,-1,0,-1,-1,0,0,-1,-2,-1,0,0,0,0,0},{-1,0,1,0,0,-1,1,0,-1,-1,0,0,0,0,0,0},{-1,-1,0,0,1,0,1,0,0,-1,-1,0,0,0,0,0},{-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{-1,1,0,0,-1,0,-1,0,0,1,-1,0,0,0,0,0},{-1,0,-1,0,0,-1,-1,0,1,1,0,0,0,0,0,0},{-2,-1,-1,0,-1,-1,-2,0,-1,0,-1,0,0,0,0,0},{0,1,1,0,1,1,0,0,1,0,1,0,0,0,0,0},{1,2,1,0,0,1,-1,0,1,1,0,0,0,0,0,0},{1,1,0,0,1,0,-1,0,2,1,1,0,0,0,0,0},{0,0,0,0,0,0,-1,0,0,0,0,0,0,0,0,0},{-1,1,0,0,1,2,1,0,0,1,1,0,0,0,0,0},{-1,0,1,0,0,1,1,0,1,1,2,0,0,0,0,0},{0,1,0,0,0,1,0,0,0,1,0,0,0,0,0,0},{0,1,-1,0,1,1,0,0,1,2,1,0,0,0,0,0},{-1,0,-1,0,0,1,-1,0,-1,1,0,0,0,0,0,0},{0,1,1,0,-1,1,0,0,1,2,1,0,0,0,0,0},{0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0},{-1,-1,0,0,-1,0,-1,0,0,1,1,0,0,0,0,0},{1,0,1,0,2,1,1,0,1,-1,0,0,0,0,0,0},{1,1,2,0,1,0,1,0,0,-1,1,0,0,0,0,0},{0,0,0,0,0,0,0,0,0,-1,0,0,0,0,0,0},{2,1,1,0,1,1,0,0,1,0,-1,0,0,0,0,0},{1,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0},{1,-1,0,0,1,0,-1,0,0,-1,-1,0,0,0,0,0},{1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0},{1,1,0,0,-1,0,-1,0,0,-1,-1,0,0,0,0,0},{2,1,1,0,1,-1,0,0,1,0,1,0,0,0,0,0},{1,0,1,0,0,-1,-1,0,-1,-1,0,0,0,0,0,0},{1,0,-1,0,0,-1,-1,0,1,-1,0,0,0,0,0,0},{0,-1,-1,0,-1,-1,-2,0,-1,-2,-1,0,0,0,0,0},{0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0},{0,1,1,0,1,1,2,0,-1,0,1,0,0,0,0,0},{-1,0,-1,0,0,1,1,0,-1,-1,0,0,0,0,0,0},{1,1,0,0,1,2,1,0,0,1,-1,0,0,0,0,0},{1,0,-1,0,2,1,1,0,1,1,0,0,0,0,0,0},{0,-1,-1,0,1,1,0,0,-1,0,-1,0,0,0,0,0},{1,2,1,0,0,1,1,0,-1,1,0,0,0,0,0,0},{0,1,-1,0,-1,1,0,0,-1,0,-1,0,0,0,0,0},{1,1,0,0,1,0,1,0,0,1,1,0,0,0,0,0},{0,0,0,0,0,0,0,0,-1,0,0,0,0,0,0,0},{0,0,-1,0,0,0,0,0,0,0,0,0,0,0,0,0},{-1,-1,-2,0,-1,0,-1,0,-2,-1,-1,0,0,0,0,0},{0,-1,1,0,1,1,2,0,1,0,1,0,0,0,0,0},{0,0,1,0,0,0,1,0,0,0,1,0,0,0,0,0},{-1,-1,0,0,-1,0,1,0,0,-1,1,0,0,0,0,0},{1,0,1,0,0,1,1,0,1,1,0,0,0,0,0,0},{1,-1,0,0,1,0,1,0,2,1,1,0,0,0,0,0},{0,-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{1,1,2,0,-1,0,1,0,0,1,1,0,0,0,0,0},{0,0,0,0,-1,0,0,0,0,0,0,0,0,0,0,0},{1,0,1,0,0,-1,1,0,1,1,2,0,0,0,0,0},{0,-1,1,0,-1,-1,0,0,-1,0,1,0,0,0,0,0},{0,-1,-1,0,-1,-1,0,0,1,0,1,0,0,0,0,0},{-1,-2,-1,0,-2,-1,-1,0,-1,-1,0,0,0,0,0,0}};
ineqrhsPd = matrix {{-2},{-2},{-2},{0},{-2},{0},{-2},{0},{0},{-2},{0},{0},{0},{0},{0},{-2},{2},{2},{2},{0},{2},{2},{1},{2},{0},{2},{1},{0},{2},{2},{0},{2},{1},{0},{1},{0},{2},{0},{0},{-2},{1},{2},{0},{2},{2},{0},{2},{0},{2},{0},{0},{-2},{2},{1},{0},{2},{2},{0},{2},{0},{2},{0},{0},{-2}};
eqlhsPd = matrix {{-1,-1,-1,-1,0,0,0,0,0,0,0,0,0,0,0,0},{0,0,0,0,-1,-1,-1,-1,0,0,0,0,0,0,0,0},{0,0,0,0,0,0,0,0,-1,-1,-1,-1,0,0,0,0},{-3/4,1/4,1/4,1/4,-3/4,1/4,1/4,1/4,-3/4,1/4,1/4,1/4,-1,0,0,0},{1/13,-9/13,4/13,4/13,1/13,-9/13,4/13,4/13,1/13,-9/13,4/13,4/13,-3/13,-1,0,0},{1/10,1/10,-3/5,2/5,1/10,1/10,-3/5,2/5,1/10,1/10,-3/5,2/5,-3/10,-3/10,-1,0},{1/7,1/7,1/7,-3/7,1/7,1/7,1/7,-3/7,1/7,1/7,1/7,-3/7,-3/7,-3/7,-3/7,-1}};
eqrhsPd = matrix {{-1},{-1},{-1},{-1/4},{-4/13},{-2/5},{-4/7}};
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 1/5670)
assert(volume Pd == 1/5670)
LE = reverse {1,383/126,1199/315,29797/11340,9/8,173/540,1/15,23/1890,1/504,1/5670};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test split_polyhedron/1.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{2,4},{4,2},{4,2},{2,4}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,-2,0,0},{0,0,0,-2}};
ineqrhsPd = matrix {{-4},{-4}};
eqlhsPd = matrix {{-1,-1,-1,-1},{1,-1,1,-1},{1,1,-1,-1}};
eqrhsPd = matrix {{-12},{0},{0}};
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 2)
assert(volume Pd == 2)
LE = reverse {1,2};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test flag_vector/1.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1},{-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1},{-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1}};
raysP = map(QQ^5, QQ^0, 0);
linealityP = map(QQ^5, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,0,0,0,0},{1,0,0,0,0},{0,-1,0,0,0},{0,1,0,0,0},{0,0,-1,0,0},{0,0,1,0,0},{0,0,0,-1,0},{0,0,0,1,0},{0,0,0,0,-1},{0,0,0,0,1}};
ineqrhsPd = matrix {{1},{1},{1},{1},{1},{1},{1},{1},{1},{1}};
eqlhsPd = map(QQ^0, QQ^5, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 32)
assert(volume Pd == 32)
LE = reverse {1,10,40,80,80,32};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test flag_vector/2.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,1,0,0,0,0,0,0},{0,0,1,0,0,0,0,0},{0,0,0,1,0,0,0,0},{0,0,0,0,1,0,0,0},{0,0,0,0,0,1,0,0},{0,0,0,0,0,0,1,0},{0,0,0,0,0,0,0,1}};
raysP = map(QQ^7, QQ^0, 0);
linealityP = map(QQ^7, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,1,1,1,1,1,1},{-1,0,0,0,0,0,0},{0,-1,0,0,0,0,0},{0,0,-1,0,0,0,0},{0,0,0,-1,0,0,0},{0,0,0,0,-1,0,0},{0,0,0,0,0,-1,0},{0,0,0,0,0,0,-1}};
ineqrhsPd = matrix {{1},{0},{0},{0},{0},{0},{0},{0}};
eqlhsPd = map(QQ^0, QQ^7, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 1/5040)
assert(volume Pd == 1/5040)
LE = reverse {1,363/140,469/180,967/720,7/18,23/360,1/180,1/5040};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test beneath_beyond/6.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,2}};
raysP = map(QQ^1, QQ^0, 0);
linealityP = map(QQ^1, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1},{1}};
ineqrhsPd = matrix {{0},{2}};
eqlhsPd = map(QQ^0, QQ^1, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 2)
assert(volume Pd == 2)
LE = reverse {1,2};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test beneath_beyond/8.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,1,0,1,0},{0,0,1,1,0},{0,0,0,0,1},{0,0,0,0,0},{0,0,0,0,0}};
raysP = map(QQ^5, QQ^0, 0);
linealityP = map(QQ^5, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,0,0,0,0},{0,0,-1,0,0},{0,-1,0,0,0},{0,1,1,0,0},{1,0,1,0,0}};
ineqrhsPd = matrix {{0},{0},{0},{1},{1}};
eqlhsPd = matrix {{0,0,0,-1,0},{0,0,0,0,-1}};
eqrhsPd = matrix {{0},{0}};
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

-- Test beneath_beyond/1r.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,-1,0,0,0,0,0,0},{0,0,1,-1,0,0,0,0},{0,0,0,0,1,-1,0,0},{0,0,0,0,0,0,1,-1}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,-1,-1,-1},{-1,1,-1,-1},{-1,-1,-1,1},{1,1,1,1},{1,-1,-1,1},{-1,1,1,-1},{1,-1,-1,-1},{-1,1,1,1},{-1,-1,1,1},{-1,1,-1,1},{1,-1,1,-1},{1,-1,1,1},{1,1,-1,1},{1,1,-1,-1},{-1,-1,1,-1},{1,1,1,-1}};
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

-- Test beneath_beyond/1.poly
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

-- Test beneath_beyond/4.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1},{-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1},{-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1},{-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1}};
raysP = map(QQ^6, QQ^0, 0);
linealityP = map(QQ^6, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,0,0,0,0,0},{0,0,-1,0,0,0},{0,-1,0,0,0,0},{0,0,1,0,0,0},{0,0,0,0,-1,0},{0,0,0,-1,0,0},{0,0,0,1,0,0},{0,1,0,0,0,0},{0,0,0,0,0,-1},{1,0,0,0,0,0},{0,0,0,0,0,1},{0,0,0,0,1,0}};
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

-- Test beneath_beyond/2.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1},{-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,0,0,0},{0,0,-1,0},{0,-1,0,0},{1,0,0,0},{0,0,0,1},{0,0,0,-1},{0,1,0,0},{0,0,1,0}};
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

-- Test beneath_beyond/5.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1},{2},{3}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,-2,-3}};
ineqrhsPd = matrix {{1}};
eqlhsPd = matrix {{-1,0,0},{0,-1,0},{0,0,-1}};
eqrhsPd = matrix {{-1},{-2},{-3}};
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

-- Test beneath_beyond/7.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,2},{0,-2}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,1},{1,-1}};
ineqrhsPd = matrix {{0},{4}};
eqlhsPd = matrix {{-1,-1}};
eqrhsPd = matrix {{0}};
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 2)
assert(volume Pd == 2)
LE = reverse {1,2};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test POINTED/1V.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,1},{1,0}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,0},{-1,0}};
ineqrhsPd = matrix {{1},{0}};
eqlhsPd = matrix {{-1,-1}};
eqrhsPd = matrix {{-1}};
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

-- Test POINTED/2F.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,1,3,4,1,2,3,2,2,3,1,1,4,4,2,1,4,3,4,4,3,3,2,2},{3,4,4,3,4,4,1,1,3,2,3,2,2,1,1,2,1,1,3,2,2,4,4,3},{4,3,1,1,2,1,2,3,1,1,2,3,1,2,4,4,3,4,2,3,4,2,3,4},{2,2,2,2,3,3,4,4,4,4,4,4,3,3,3,3,2,2,1,1,1,1,1,1}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,0,0,0},{1,1,0,0},{1,1,1,0},{0,0,-1,0},{1,0,1,0},{0,1,0,0},{-1,-1,-1,0},{-1,0,0,0},{-1,-1,0,0},{-1,0,-1,0},{0,1,1,0},{0,0,1,0},{0,-1,-1,0},{0,-1,0,0}};
ineqrhsPd = matrix {{4},{7},{9},{-1},{7},{4},{-6},{-1},{-3},{-3},{7},{4},{-3},{-1}};
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

-- Test FACET_POINT_LATTICE_DISTANCES/p.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{2,0,0,1,2},{0,0,1,2,2}};
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

-- Test FACET_POINT_LATTICE_DISTANCES/c.poly
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

-- Test FAR_FACE/1.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{-1,1,0,0},{-1,0,1,0},{-1,0,0,1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,1,1},{1,1,-3},{1,-3,1},{-3,1,1}};
ineqrhsPd = matrix {{1},{1},{1},{1}};
eqlhsPd = map(QQ^0, QQ^3, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 2/3)
assert(volume Pd == 2/3)
LE = reverse {1,7/3,1,2/3};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test fractional_cut_polytope/1.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,1,1,0,0,0,1,1},{0,1,0,1,0,1,0,1},{0,0,1,1,0,1,1,0},{0,1,0,0,1,1,1,0},{0,0,1,0,1,1,0,1},{0,0,0,1,1,0,1,1}};
raysP = map(QQ^6, QQ^0, 0);
linealityP = map(QQ^6, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,-1,0,-1,0,1},{-1,0,0,-1,1,0},{-1,-1,1,0,0,0},{0,0,1,0,1,1},{0,1,0,1,0,1},{1,0,0,1,1,0},{1,1,1,0,0,0},{-1,0,0,1,-1,0},{-1,1,-1,0,0,0},{0,0,-1,0,-1,1},{0,-1,0,1,0,-1},{0,0,-1,0,1,-1},{1,-1,-1,0,0,0},{0,0,1,0,-1,-1},{0,1,0,-1,0,-1},{1,0,0,-1,-1,0}};
ineqrhsPd = matrix {{0},{0},{0},{2},{2},{2},{2},{0},{0},{0},{0},{0},{0},{0},{0},{0}};
eqlhsPd = map(QQ^0, QQ^6, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 2/45)
assert(volume Pd == 2/45)
LE = reverse {1,12/5,98/45,4/3,7/9,4/15,2/45};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test fractional_cut_polytope/2.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,1,1,0,0,0,0,0,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1},{0,0,1,1,0,0,0,1,1,0,0,0,0,1,1,1,1,1,1,0,0,0,0,1,1,1,1,1,1,0,0,0},{0,0,0,1,1,0,0,0,1,1,0,0,1,1,0,0,0,1,1,1,1,0,1,1,0,0,0,1,1,1,1,0},{0,0,1,0,0,1,0,1,0,0,1,0,1,1,0,1,0,1,0,1,0,1,1,1,0,1,0,1,0,1,0,1},{0,0,0,0,1,1,0,0,0,1,1,0,0,1,1,0,1,1,0,0,1,1,0,1,1,0,1,1,0,0,1,1},{0,0,0,0,0,1,1,0,0,0,1,1,0,0,1,1,0,1,1,1,1,0,0,0,1,1,0,1,1,1,1,0}};
raysP = map(QQ^6, QQ^0, 0);
linealityP = map(QQ^6, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,0,0,0,0,-1},{-1,0,0,0,0,0},{0,-1,0,0,0,0},{0,-1,-1,-1,1,0},{0,-1,-1,1,-1,0},{0,0,-1,0,0,0},{0,-1,1,-1,-1,0},{0,0,0,-1,0,0},{0,0,0,0,-1,0},{0,1,-1,-1,-1,0},{0,0,0,0,0,1},{0,-1,1,1,1,0},{0,0,0,0,1,0},{0,0,0,1,0,0},{0,1,-1,1,1,0},{0,0,1,0,0,0},{0,1,1,-1,1,0},{0,1,1,1,-1,0},{0,1,0,0,0,0},{1,0,0,0,0,0}};
ineqrhsPd = matrix {{0},{0},{0},{0},{0},{0},{0},{0},{0},{0},{1},{2},{1},{1},{2},{1},{2},{2},{1},{1}};
eqlhsPd = map(QQ^0, QQ^6, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 2/3)
assert(volume Pd == 2/3)
LE = reverse {1,4,22/3,26/3,7,10/3,2/3};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test CD_INDEX/1.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,1}};
raysP = map(QQ^1, QQ^0, 0);
linealityP = map(QQ^1, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1},{-1}};
ineqrhsPd = matrix {{1},{0}};
eqlhsPd = map(QQ^0, QQ^1, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
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

-- Test CD_INDEX/4.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,1,0,0,0,0,0,0,0},{0,0,1,0,0,0,0,0,0},{0,0,0,1,0,0,0,0,0},{0,0,0,0,1,0,0,0,0},{0,0,0,0,0,1,0,0,0},{0,0,0,0,0,0,1,0,0},{0,0,0,0,0,0,0,1,0},{0,0,0,0,0,0,0,0,1}};
raysP = map(QQ^8, QQ^0, 0);
linealityP = map(QQ^8, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,1,1,1,1,1,1,1},{-1,0,0,0,0,0,0,0},{0,-1,0,0,0,0,0,0},{0,0,-1,0,0,0,0,0},{0,0,0,-1,0,0,0,0},{0,0,0,0,-1,0,0,0},{0,0,0,0,0,-1,0,0},{0,0,0,0,0,0,-1,0},{0,0,0,0,0,0,0,-1}};
ineqrhsPd = matrix {{1},{0},{0},{0},{0},{0},{0},{0},{0}};
eqlhsPd = map(QQ^0, QQ^8, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 1/40320)
assert(volume Pd == 1/40320)
LE = reverse {1,761/280,29531/10080,267/160,1069/1920,9/80,13/960,1/1120,1/40320};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test CD_INDEX/2.poly
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

-- Test truncated_orbit_polytope/1-in.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,0,1,-1,0,0},{0,1,0,0,-1,0},{1,0,0,0,0,-1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,1,-1},{-1,1,1},{1,1,1},{-1,-1,1},{1,-1,1},{1,1,-1},{-1,-1,-1},{1,-1,-1}};
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

-- Test VISUAL_DIRECTED_GRAPH/1.poly
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

-- Test DIM/1V.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{2,-2,5,0},{3,3,4,2},{4,-4,-3,0},{5,5,-2,-4}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{11,-26,1,0},{-2,13,1,0},{1,-26/5,-9/5,0},{-2,0,1,0}};
ineqrhsPd = matrix {{-52},{39},{-52/5},{0}};
eqlhsPd = matrix {{-32/13,9,16/13,-1}};
eqrhsPd = matrix {{22}};
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 2/3)
assert(volume Pd == 2/3)
LE = reverse {1,17/6,5/2,2/3};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test lattice_normalization/1v.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,-2,1,0},{0,-1,0,-1}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,2},{0,1},{1,-1},{0,-1}};
ineqrhsPd = matrix {{0},{0},{1},{1}};
eqlhsPd = map(QQ^0, QQ^2, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 3/2)
assert(volume Pd == 3/2)
LE = reverse {1,5/2,3/2};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test lattice_normalization/2v.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,0,0,0,-5,-1,-5,-1},{0,1,1,0,1,0,1,0},{0,0,1,1,1,1,0,0}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,1,0},{0,0,-1},{0,0,1},{0,-1,0},{1,0,0},{-1,-4,0}};
ineqrhsPd = matrix {{1},{0},{1},{0},{0},{1}};
eqlhsPd = map(QQ^0, QQ^3, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 3)
assert(volume Pd == 3)
LE = reverse {1,5,7,3};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test lattice_normalization/2-in.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,1,1,1,1,1,1,1},{-1,-1,1,1,1,1,-1,-1},{1,1,1,1,-4,0,-4,0},{-1,1,1,-1,1,-1,1,-1}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,0,0,1},{0,-1,0,0},{0,1,0,0},{0,0,0,-1},{0,0,1,0},{0,0,-1/2,-1}};
ineqrhsPd = matrix {{1},{1},{1},{1},{1},{1}};
eqlhsPd = matrix {{1,0,0,0}};
eqrhsPd = matrix {{1}};
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 12)
assert(volume Pd == 12)
LE = reverse {1,7,16,12};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test lattice_normalization/1-in.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{2,6,2,6},{0,-2,1,0},{1,1,-1,-3}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,-2,0},{-1,0,0},{1,4,0},{1,0,0}};
ineqrhsPd = matrix {{-2},{-2},{6},{6}};
eqlhsPd = matrix {{-1,-2,-1}};
eqrhsPd = matrix {{-3}};
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 6)
assert(volume Pd == 6)
LE = reverse {1,3,6};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test lattice_normalization/1a.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,-2,1,0},{0,-4,0,-4}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-2,1},{0,1},{4,-1},{0,-1}};
ineqrhsPd = matrix {{0},{0},{4},{4}};
eqlhsPd = map(QQ^0, QQ^2, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 6)
assert(volume Pd == 6)
LE = reverse {1,3,6};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test lattice_normalization/2a.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,0,0,0,-5,-1,-5,-1},{0,2,2,0,2,0,2,0},{0,0,2,2,2,2,0,0}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,1,0},{0,0,-1},{0,0,1},{0,-1,0},{1,0,0},{-1,-2,0}};
ineqrhsPd = matrix {{2},{0},{2},{0},{0},{1}};
eqlhsPd = map(QQ^0, QQ^3, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 12)
assert(volume Pd == 12)
LE = reverse {1,7,16,12};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test COUNTING/1V.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,1,0},{0,0,1}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,-1},{-1,0},{1,1}};
ineqrhsPd = matrix {{0},{0},{1}};
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

-- Test COUNTING/1F.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,1,0,1},{0,0,1,1}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,0},{0,-1},{1,0},{0,1}};
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

-- Test edge_directions/3.poly
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

-- Test cayley_embedding/1-A.poly
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

-- Test cayley_embedding/1.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{-1,1,-1,1,-1,1,-1,1,-1,0,-1,-1},{-1,-1,1,1,-1,-1,1,1,-1,-1,0,-1},{-1,-1,-1,-1,1,1,1,1,-1,-1,-1,0},{1,1,1,1,1,1,1,1,0,0,0,0},{0,0,0,0,0,0,0,0,1,1,1,1}};
raysP = map(QQ^5, QQ^0, 0);
linealityP = map(QQ^5, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,0,-1,0,0},{0,0,0,-1,0},{-1,0,0,0,0},{0,-1,0,0,0},{1,1,1,-5,0},{0,1,1,-3,0},{1,0,1,-3,0},{1,1,0,-3,0},{0,0,1,-1,0},{0,1,0,-1,0},{1,0,0,-1,0},{0,0,0,1,0}};
ineqrhsPd = matrix {{1},{0},{1},{1},{-2},{-1},{-1},{-1},{0},{0},{0},{1}};
eqlhsPd = matrix {{0,0,0,-1,-1}};
eqrhsPd = matrix {{-1}};
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 79/24)
assert(volume Pd == 79/24)
LE = reverse {1,67/12,269/24,119/12,79/24};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test cayley_embedding/1-B.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{-1,0,-1,-1},{-1,-1,0,-1},{-1,-1,-1,0}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,1,1},{-1,0,0},{0,-1,0},{0,0,-1}};
ineqrhsPd = matrix {{-2},{1},{1},{1}};
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

-- Test FacetPerm/1.poly
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

-- Test FacetPerm/2p.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,0,1,0,1,1,1,0,1,1},{1,0,0,0,1,1,0,0,1,1},{0,1,0,0,0,0,0,1,0,1},{1,1,0,1,0,1,0,1,0,1},{0,0,0,1,0,1,1,1,1,1}};
raysP = map(QQ^5, QQ^0, 0);
linealityP = map(QQ^5, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,-1,0,1,0},{0,0,0,0,1},{-1,0,-1,0,-1},{0,0,-1,0,0},{1,0,0,0,0},{-1,1,0,0,0},{-1,0,0,-1,0},{1,0,1,0,-1},{0,0,1,-1,0}};
ineqrhsPd = matrix {{1},{1},{-1},{0},{1},{0},{-1},{1},{0}};
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

-- Test FacetPerm/1p.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,0,0},{0,-1,0},{0,0,-1},{1,0,0},{0,1,0},{0,0,1}};
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

-- Test FacetPerm/2.poly
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

-- Test cast/6.poly
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

-- Test cast/5.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,2,1,0,2},{2,0,-1,0,2}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,0},{-1,-1},{0,1},{1,0},{1,-1}};
ineqrhsPd = matrix {{0},{0},{2},{2},{2}};
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

-- Test wreath/2-B.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{-1,0,1},{-1,1,0}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,1},{1,-2},{-2,1}};
ineqrhsPd = matrix {{1},{1},{1}};
eqlhsPd = map(QQ^0, QQ^2, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 3/2)
assert(volume Pd == 3/2)
LE = reverse {1,3/2,3/2};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test wreath/1-A.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{-1,0,1},{-1,1,0}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,1},{1,-2},{-2,1}};
ineqrhsPd = matrix {{1},{1},{1}};
eqlhsPd = map(QQ^0, QQ^2, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 3/2)
assert(volume Pd == 3/2)
LE = reverse {1,3/2,3/2};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test wreath/1.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{-1,0,1,0,0,0,0,0,0,0,0,0},{-1,1,0,0,0,0,0,0,0,0,0,0},{0,0,0,-1,0,1,0,0,0,0,0,0},{0,0,0,-1,1,0,0,0,0,0,0,0},{0,0,0,0,0,0,-1,0,1,0,0,0},{0,0,0,0,0,0,-1,1,0,0,0,0},{0,0,0,0,0,0,0,0,0,-1,0,1},{0,0,0,0,0,0,0,0,0,-1,1,0},{-1,-1,-1,-1,-1,-1,1,1,1,1,1,1},{-1,-1,-1,1,1,1,1,1,1,-1,-1,-1}};
raysP = map(QQ^10, QQ^0, 0);
linealityP = map(QQ^10, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-4,2,-4,2,0,0,0,0,1,0},{-4,2,0,0,0,0,-4,2,0,1},{-4,2,0,0,0,0,2,-4,0,1},{-4,2,0,0,0,0,2,2,0,1},{-4,2,2,-4,0,0,0,0,1,0},{-4,2,2,2,0,0,0,0,1,0},{2,2,2,2,0,0,0,0,1,0},{2,2,2,-4,0,0,0,0,1,0},{2,2,0,0,0,0,2,2,0,1},{2,2,0,0,0,0,2,-4,0,1},{2,2,0,0,0,0,-4,2,0,1},{2,2,-4,2,0,0,0,0,1,0},{2,-4,2,2,0,0,0,0,1,0},{0,0,2,2,2,2,0,0,0,-1},{0,0,2,2,2,-4,0,0,0,-1},{0,0,2,2,-4,2,0,0,0,-1},{2,-4,2,-4,0,0,0,0,1,0},{2,-4,0,0,0,0,2,2,0,1},{2,-4,0,0,0,0,2,-4,0,1},{2,-4,0,0,0,0,-4,2,0,1},{2,-4,-4,2,0,0,0,0,1,0},{0,0,2,-4,2,2,0,0,0,-1},{0,0,0,0,2,2,2,2,-1,0},{0,0,0,0,2,2,2,-4,-1,0},{0,0,0,0,2,2,-4,2,-1,0},{0,0,-4,2,2,2,0,0,0,-1},{0,0,2,-4,2,-4,0,0,0,-1},{0,0,2,-4,-4,2,0,0,0,-1},{0,0,0,0,2,-4,2,2,-1,0},{0,0,0,0,-4,2,2,2,-1,0},{0,0,0,0,2,-4,2,-4,-1,0},{0,0,0,0,2,-4,-4,2,-1,0},{0,0,-4,2,2,-4,0,0,0,-1},{0,0,0,0,-4,2,2,-4,-1,0},{0,0,0,0,-4,2,-4,2,-1,0},{0,0,-4,2,-4,2,0,0,0,-1}};
ineqrhsPd = matrix {{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1}};
eqlhsPd = map(QQ^0, QQ^10, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 1/5600)
assert(volume Pd == 1/5600)
LE = reverse {1,1019/280,33651/5600,1145/224,15689/4480,189/160,753/1600,15/224,81/4480,1/1120,1/5600};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test wreath/4-B.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{-1,0,1},{-1,1,0}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,1},{1,-2},{-2,1}};
ineqrhsPd = matrix {{1},{1},{1}};
eqlhsPd = map(QQ^0, QQ^2, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 3/2)
assert(volume Pd == 3/2)
LE = reverse {1,3/2,3/2};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test wreath/1-B.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{-1,-1,1,1},{-1,1,1,-1}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,-1},{-1,0},{0,1},{1,0}};
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

-- Test wreath/2.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{-1,0,1,0,0,0,0,0,0},{-1,1,0,0,0,0,0,0,0},{0,0,0,-1,0,1,0,0,0},{0,0,0,-1,1,0,0,0,0},{0,0,0,0,0,0,-1,0,1},{0,0,0,0,0,0,-1,1,0},{-1,-1,-1,0,0,0,1,1,1},{-1,-1,-1,1,1,1,0,0,0}};
raysP = map(QQ^8, QQ^0, 0);
linealityP = map(QQ^8, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{3,3,0,0,0,0,1,1},{0,0,3,3,0,0,1,-2},{3,-6,0,0,0,0,1,1},{0,0,0,0,3,3,-2,1},{0,0,3,-6,0,0,1,-2},{0,0,0,0,3,-6,-2,1},{0,0,0,0,-6,3,-2,1},{0,0,-6,3,0,0,1,-2},{-6,3,0,0,0,0,1,1}};
ineqrhsPd = matrix {{1},{1},{1},{1},{1},{1},{1},{1},{1}};
eqlhsPd = map(QQ^0, QQ^8, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 9/4480)
assert(volume Pd == 9/4480)
LE = reverse {1,843/280,4791/1120,423/160,1023/640,27/80,39/320,9/1120,9/4480};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test wreath/2-A.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{-1,0,1},{-1,1,0}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,1},{1,-2},{-2,1}};
ineqrhsPd = matrix {{1},{1},{1}};
eqlhsPd = map(QQ^0, QQ^2, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 3/2)
assert(volume Pd == 3/2)
LE = reverse {1,3/2,3/2};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test N_FLAGS/1.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,1}};
raysP = map(QQ^1, QQ^0, 0);
linealityP = map(QQ^1, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1},{-1}};
ineqrhsPd = matrix {{1},{0}};
eqlhsPd = map(QQ^0, QQ^1, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
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

-- Test N_FLAGS/2.poly
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

-- Test N_FLAGS/5.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,1,0,0,0,0,0,0,0},{0,0,1,0,0,0,0,0,0},{0,0,0,1,0,0,0,0,0},{0,0,0,0,1,0,0,0,0},{0,0,0,0,0,1,0,0,0},{0,0,0,0,0,0,1,0,0},{0,0,0,0,0,0,0,1,0},{0,0,0,0,0,0,0,0,1}};
raysP = map(QQ^8, QQ^0, 0);
linealityP = map(QQ^8, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,1,1,1,1,1,1,1},{-1,0,0,0,0,0,0,0},{0,-1,0,0,0,0,0,0},{0,0,-1,0,0,0,0,0},{0,0,0,-1,0,0,0,0},{0,0,0,0,-1,0,0,0},{0,0,0,0,0,-1,0,0},{0,0,0,0,0,0,-1,0},{0,0,0,0,0,0,0,-1}};
ineqrhsPd = matrix {{1},{0},{0},{0},{0},{0},{0},{0},{0}};
eqlhsPd = map(QQ^0, QQ^8, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 1/40320)
assert(volume Pd == 1/40320)
LE = reverse {1,761/280,29531/10080,267/160,1069/1920,9/80,13/960,1/1120,1/40320};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test neighbors_cyclic_normal/4.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,0,1,1},{0,1,0,1},{0,0,0,0}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,-1,0},{-1,0,0},{0,1,0},{1,0,0}};
ineqrhsPd = matrix {{0},{0},{1},{1}};
eqlhsPd = matrix {{0,0,-1}};
eqrhsPd = matrix {{0}};
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

-- Test neighbors_cyclic_normal/3.poly
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

-- Test BOUNDED/1.poly
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

-- Test BOUNDED/1F.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,10},{-1,-1}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,0},{1,0}};
ineqrhsPd = matrix {{-1},{10}};
eqlhsPd = matrix {{0,-1}};
eqrhsPd = matrix {{1}};
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 9)
assert(volume Pd == 9)
LE = reverse {1,9};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test orbit_polytope/1.poly
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

-- Test orbit_polytope/2.poly
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

-- Test orbit_polytope/3.poly
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

-- Test Scheduler/1dis_ex.poly
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

-- Test Scheduler/6.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,3,0,0,0,0,0,-3},{0,0,3,0,0,0,-3,0},{0,0,0,3,0,-3,0,0},{3,0,0,0,-3,0,0,0}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,1,1,1},{-1,1,1,1},{1,-1,1,1},{-1,-1,1,1},{1,1,-1,1},{-1,1,-1,1},{1,-1,-1,1},{-1,-1,-1,1},{1,1,1,-1},{-1,1,1,-1},{1,-1,1,-1},{-1,-1,1,-1},{1,1,-1,-1},{-1,1,-1,-1},{1,-1,-1,-1},{-1,-1,-1,-1}};
ineqrhsPd = matrix {{3},{3},{3},{3},{3},{3},{3},{3},{3},{3},{3},{3},{3},{3},{3},{3}};
eqlhsPd = map(QQ^0, QQ^4, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 54)
assert(volume Pd == 54)
LE = reverse {1,8,30,36,54};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test Scheduler/8.poly
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

-- Test Scheduler/9rat.poly
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

-- Test Scheduler/1app-in.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,-1,0,0,0,0},{0,0,1,-1,0,0},{0,0,0,0,1,-1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,1,-1},{-1,1,1},{-1,-1,1},{-1,-1,-1},{1,-1,1},{1,-1,-1},{1,1,1},{1,1,-1}};
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

-- Test Scheduler/2ov_p.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,1,1},{0,0,1}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,0},{-1,1},{0,-1}};
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

-- Test Scheduler/2.poly
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

-- Test Scheduler/5.poly
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

-- Test Scheduler/3.poly
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

-- Test Scheduler/1app.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,-1,0,0,0,0},{0,0,1,-1,0,0},{0,0,0,0,1,-1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,1,-1},{-1,1,1},{-1,-1,1},{-1,-1,-1},{1,-1,1},{1,-1,-1},{1,1,1},{1,1,-1}};
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

-- Test Scheduler/7.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,0,1,1},{0,1,0,1}};
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

-- Test REL_INT_POINT/1.poly
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

-- Test REL_INT_POINT/2.poly
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

-- Test volume/6.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,1,0,1,0,1,0,1},{0,0,1,1,0,0,1,1},{0,0,0,0,1,1,1,1},{0,0,0,0,0,0,0,0},{0,0,0,0,0,0,0,0},{0,0,0,0,0,0,0,0},{0,0,0,0,0,0,0,0},{0,0,0,0,0,0,0,0}};
raysP = map(QQ^8, QQ^0, 0);
linealityP = map(QQ^8, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,0,-1,0,0,0,0,0},{-1,0,0,0,0,0,0,0},{0,-1,0,0,0,0,0,0},{0,0,1,0,0,0,0,0},{0,1,0,0,0,0,0,0},{1,0,0,0,0,0,0,0}};
ineqrhsPd = matrix {{0},{0},{0},{1},{1},{1}};
eqlhsPd = matrix {{0,0,0,-1,0,0,0,0},{0,0,0,0,-1,0,0,0},{0,0,0,0,0,-1,0,0},{0,0,0,0,0,0,-1,0},{0,0,0,0,0,0,0,-1}};
eqrhsPd = matrix {{0},{0},{0},{0},{0}};
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

-- Test volume/8.poly
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

-- Test volume/1.poly
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

-- Test volume/2.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1},{-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,0,0,0},{0,0,-1,0},{0,-1,0,0},{1,0,0,0},{0,0,0,1},{0,0,0,-1},{0,1,0,0},{0,0,1,0}};
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

-- Test volume/5.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,-1,1,-1},{0,0,2,2}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,-1},{-1,0},{1,0},{0,1}};
ineqrhsPd = matrix {{0},{1},{1},{2}};
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

-- Test volume/3.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0},{1,1,1,1,0,0,0,0,0,0,1,1,1,1,1,1,0,0,0,0},{1,0,0,0,1,1,1,0,0,0,1,1,1,0,0,0,1,1,1,0},{0,1,0,0,1,0,0,1,1,0,1,0,0,1,1,0,1,1,0,1},{0,0,1,0,0,1,0,1,0,1,0,1,0,1,0,1,1,0,1,1},{0,0,0,1,0,0,1,0,1,1,0,0,1,0,1,1,0,1,1,1}};
raysP = map(QQ^6, QQ^0, 0);
linealityP = map(QQ^6, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,-1,5,-1,-1,-1},{-1,-1,-1,5,-1,-1},{1,1,1,1,-5,1},{1/5,1/5,1/5,1/5,1/5,-1},{-1,5,-1,-1,-1,-1},{-1,-1,-1,-1,5,-1},{1,1,-5,1,1,1},{1,-5,1,1,1,1},{5,-1,-1,-1,-1,-1},{-1/5,-1/5,-1/5,-1/5,-1/5,1},{1,1,1,-5,1,1},{-5,1,1,1,1,1}};
ineqrhsPd = matrix {{3},{3},{3},{3/5},{3},{3},{3},{3},{3},{3/5},{3},{3}};
eqlhsPd = matrix {{-1,-1,-1,-1,-1,-1}};
eqrhsPd = matrix {{-3}};
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 11/20)
assert(volume Pd == 11/20)
LE = reverse {1,37/10,25/4,23/4,11/4,11/20};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test volume/7.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,1,0,1,0,1,0,1},{0,0,1,1,0,0,1,1},{0,0,0,0,1,1,1,1},{0,0,0,0,0,0,0,0},{0,0,0,0,0,0,0,0},{0,0,0,0,0,0,0,0},{0,0,0,0,0,0,0,0},{0,0,0,0,0,0,0,0}};
raysP = map(QQ^8, QQ^0, 0);
linealityP = map(QQ^8, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,0,0,0,0,0,0,0},{0,0,-1,0,0,0,0,0},{0,-1,0,0,0,0,0,0},{0,1,0,0,0,0,0,0},{1,0,0,0,0,0,0,0},{0,0,1,0,0,0,0,0}};
ineqrhsPd = matrix {{0},{0},{0},{1},{1},{1}};
eqlhsPd = matrix {{0,0,0,-1,0,0,0,0},{0,0,0,0,-1,0,0,0},{0,0,0,0,0,-1,0,0},{0,0,0,0,0,0,-1,0},{0,0,0,0,0,0,0,-1}};
eqrhsPd = matrix {{0},{0},{0},{0},{0}};
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

-- Test face/c.poly
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

-- Test poly2metric/1-in.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,1,0,1},{0,0,1,1}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,0},{1,0},{0,-1},{0,1}};
ineqrhsPd = matrix {{0},{1},{0},{1}};
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

-- Test integer_hull/3-0.poly
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

-- Test integer_hull/1.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{7,29,40,40},{7,7,7,7},{0,22,0,11},{2,2,2,2}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,0,1,0},{0,0,-1,0},{1,0,0,0},{1,0,1,0}};
ineqrhsPd = matrix {{-7},{0},{40},{51}};
eqlhsPd = matrix {{0,-1,0,0},{0,0,0,-1}};
eqrhsPd = matrix {{-7},{-2}};
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 847/2)
assert(volume Pd == 847/2)
LE = reverse {1,77/2,847/2};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test integer_hull/3.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{-1,-1,-1,-1,1,1,1,1},{-1,-1,1,1,-1,-1,1,1},{-1,1,-1,1,-1,1,-1,1}};
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

-- Test NEIGHBORLY/6.poly
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

-- Test NEIGHBORLY/1.poly
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

-- Test NEIGHBORLY/3.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,1,-1,-1,-1,-1},{0,0,2,2,-2,-2},{-1,1,-1,1,-1,1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,0,0},{1,-1,0},{1,1,0},{0,0,-1},{0,0,1}};
ineqrhsPd = matrix {{1},{1},{1},{1},{1}};
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

-- Test VISUAL_FACE_LATTICE_MIN_MAX_FACE/1.poly
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

-- Test VISUAL_GRAPH_MIN_MAX_FACE/1.poly
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

-- Test stellar_indep_faces/2-in.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,1,0,0,0},{0,0,1,0,0},{0,0,0,1,0},{0,0,0,0,1}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,1,1,1},{-1,0,0,0},{0,-1,0,0},{0,0,-1,0},{0,0,0,-1}};
ineqrhsPd = matrix {{1},{0},{0},{0},{0}};
eqlhsPd = map(QQ^0, QQ^4, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 1/24)
assert(volume Pd == 1/24)
LE = reverse {1,25/12,35/24,5/12,1/24};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test stellar_indep_faces/1-in.poly
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

-- Test stellar_indep_faces/3-in.poly
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

-- Test mixed_integer_hull/1.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{-1,-1,-1,-1,1,1,1,1},{1,1,-1,-1,1,1,-1,-1},{-1,1,1,-1,-1,1,1,-1}};
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

-- Test mixed_integer_hull/3.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,1,2,2},{1,4,2,4}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,-1},{-1,0},{0,1},{1,0}};
ineqrhsPd = matrix {{0},{-1},{4},{2}};
eqlhsPd = map(QQ^0, QQ^2, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 5/2)
assert(volume Pd == 5/2)
LE = reverse {1,7/2,5/2};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test mixed_integer_hull/1-0.poly
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

-- Test COCUBICALITY/1.poly
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

-- Test COCUBICALITY/2.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,-1,0,0,0,0,0,0,0,0,0,0},{0,0,1,-1,0,0,0,0,0,0,0,0},{0,0,0,0,1,-1,0,0,0,0,0,0},{0,0,0,0,0,0,1,-1,0,0,0,0},{0,0,0,0,0,0,0,0,1,-1,0,0},{0,0,0,0,0,0,0,0,0,0,1,-1}};
raysP = map(QQ^6, QQ^0, 0);
linealityP = map(QQ^6, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,1,1,1,1,1},{-1,1,1,1,1,1},{1,-1,1,1,1,1},{-1,-1,1,1,1,1},{1,1,-1,1,1,1},{-1,1,-1,1,1,1},{1,-1,-1,1,1,1},{-1,-1,-1,1,1,1},{1,1,1,-1,1,1},{-1,1,1,-1,1,1},{1,-1,1,-1,1,1},{-1,-1,1,-1,1,1},{1,1,-1,-1,1,1},{-1,1,-1,-1,1,1},{1,-1,-1,-1,1,1},{-1,-1,-1,-1,1,1},{1,1,1,1,-1,1},{-1,1,1,1,-1,1},{1,-1,1,1,-1,1},{-1,-1,1,1,-1,1},{1,1,-1,1,-1,1},{-1,1,-1,1,-1,1},{1,-1,-1,1,-1,1},{-1,-1,-1,1,-1,1},{1,1,1,-1,-1,1},{-1,1,1,-1,-1,1},{1,-1,1,-1,-1,1},{-1,-1,1,-1,-1,1},{1,1,-1,-1,-1,1},{-1,1,-1,-1,-1,1},{1,-1,-1,-1,-1,1},{-1,-1,-1,-1,-1,1},{1,1,1,1,1,-1},{-1,1,1,1,1,-1},{1,-1,1,1,1,-1},{-1,-1,1,1,1,-1},{1,1,-1,1,1,-1},{-1,1,-1,1,1,-1},{1,-1,-1,1,1,-1},{-1,-1,-1,1,1,-1},{1,1,1,-1,1,-1},{-1,1,1,-1,1,-1},{1,-1,1,-1,1,-1},{-1,-1,1,-1,1,-1},{1,1,-1,-1,1,-1},{-1,1,-1,-1,1,-1},{1,-1,-1,-1,1,-1},{-1,-1,-1,-1,1,-1},{1,1,1,1,-1,-1},{-1,1,1,1,-1,-1},{1,-1,1,1,-1,-1},{-1,-1,1,1,-1,-1},{1,1,-1,1,-1,-1},{-1,1,-1,1,-1,-1},{1,-1,-1,1,-1,-1},{-1,-1,-1,1,-1,-1},{1,1,1,-1,-1,-1},{-1,1,1,-1,-1,-1},{1,-1,1,-1,-1,-1},{-1,-1,1,-1,-1,-1},{1,1,-1,-1,-1,-1},{-1,1,-1,-1,-1,-1},{1,-1,-1,-1,-1,-1},{-1,-1,-1,-1,-1,-1}};
ineqrhsPd = matrix {{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1}};
eqlhsPd = map(QQ^0, QQ^6, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 4/45)
assert(volume Pd == 4/45)
LE = reverse {1,46/15,196/45,8/3,14/9,4/15,4/45};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test schlegel_transform/1.poly
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

-- Test schlegel_transform/2.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1},{-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1},{-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1},{-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1}};
raysP = map(QQ^6, QQ^0, 0);
linealityP = map(QQ^6, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,0,0,0,0,-1},{-1,0,0,0,0,0},{0,-1,0,0,0,0},{0,0,-1,0,0,0},{0,0,0,-1,0,0},{0,0,0,0,-1,0},{0,0,0,0,0,1},{0,0,0,0,1,0},{0,0,0,1,0,0},{0,0,1,0,0,0},{0,1,0,0,0,0},{1,0,0,0,0,0}};
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

-- Test SCHLEGEL_DIRECTED_GRAPH/1.poly
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

-- Test VISUAL_GRAPH_VERTEX_COLORS/1.poly
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

-- Test VISUAL_GRAPH_VERTEX_COLORS/3.poly
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

-- Test hypersimplex/6.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,0},{0,1}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,0},{-1,0}};
ineqrhsPd = matrix {{1},{0}};
eqlhsPd = matrix {{-1,-1}};
eqrhsPd = matrix {{-1}};
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

-- Test hypersimplex/1.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,1,1,1,0,0,0,0,0,0},{1,0,0,0,1,1,1,0,0,0},{0,1,0,0,1,0,0,1,1,0},{0,0,1,0,0,1,0,1,0,1},{0,0,0,1,0,0,1,0,1,1}};
raysP = map(QQ^5, QQ^0, 0);
linealityP = map(QQ^5, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,0,0,0,0},{-1,0,0,0,0},{0,1,0,0,0},{0,-1,0,0,0},{0,0,1,0,0},{0,0,-1,0,0},{0,0,0,1,0},{0,0,0,-1,0},{0,0,0,0,1},{0,0,0,0,-1}};
ineqrhsPd = matrix {{1},{0},{1},{0},{1},{0},{1},{0},{1},{0}};
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

-- Test hypersimplex/4.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,1,1,1,0,0,0,0,0,0},{1,0,0,0,1,1,1,0,0,0},{0,1,0,0,1,0,0,1,1,0},{0,0,1,0,0,1,0,1,0,1},{0,0,0,1,0,0,1,0,1,1}};
raysP = map(QQ^5, QQ^0, 0);
linealityP = map(QQ^5, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,0,0,0,0},{-1,0,0,0,0},{0,1,0,0,0},{0,-1,0,0,0},{0,0,1,0,0},{0,0,-1,0,0},{0,0,0,1,0},{0,0,0,-1,0},{0,0,0,0,1},{0,0,0,0,-1}};
ineqrhsPd = matrix {{1},{0},{1},{0},{1},{0},{1},{0},{1},{0}};
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

-- Test hypersimplex/2.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,1,1,1,1,1,1,1,1,1,0,0,0,0,0},{1,1,1,1,1,1,0,0,0,0,1,1,1,1,0},{1,1,1,0,0,0,1,1,1,0,1,1,1,0,1},{1,0,0,1,1,0,1,1,0,1,1,1,0,1,1},{0,1,0,1,0,1,1,0,1,1,1,0,1,1,1},{0,0,1,0,1,1,0,1,1,1,0,1,1,1,1}};
raysP = map(QQ^6, QQ^0, 0);
linealityP = map(QQ^6, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,0,0,0,0,0},{-1,0,0,0,0,0},{0,1,0,0,0,0},{0,-1,0,0,0,0},{0,0,1,0,0,0},{0,0,-1,0,0,0},{0,0,0,1,0,0},{0,0,0,-1,0,0},{0,0,0,0,1,0},{0,0,0,0,-1,0},{0,0,0,0,0,1},{0,0,0,0,0,-1}};
ineqrhsPd = matrix {{1},{0},{1},{0},{1},{0},{1},{0},{1},{0},{1},{0}};
eqlhsPd = matrix {{-1,-1,-1,-1,-1,-1}};
eqrhsPd = matrix {{-4}};
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 13/60)
assert(volume Pd == 13/60)
LE = reverse {1,101/30,5,47/12,3/2,13/60};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test hypersimplex/3.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0},{1,1,1,1,0,0,0,0,0,0,1,1,1,1,1,1,0,0,0,0},{1,0,0,0,1,1,1,0,0,0,1,1,1,0,0,0,1,1,1,0},{0,1,0,0,1,0,0,1,1,0,1,0,0,1,1,0,1,1,0,1},{0,0,1,0,0,1,0,1,0,1,0,1,0,1,0,1,1,0,1,1},{0,0,0,1,0,0,1,0,1,1,0,0,1,0,1,1,0,1,1,1}};
raysP = map(QQ^6, QQ^0, 0);
linealityP = map(QQ^6, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,0,-1,0,0,0},{0,0,0,1,0,0},{-1,-1,-1,-1,-1,0},{-1,0,0,0,0,0},{0,-1,0,0,0,0},{0,0,0,0,1,0},{0,0,0,-1,0,0},{0,0,1,0,0,0},{0,0,0,0,-1,0},{1,1,1,1,1,0},{0,1,0,0,0,0},{1,0,0,0,0,0}};
ineqrhsPd = matrix {{0},{1},{-2},{0},{0},{1},{0},{1},{0},{3},{1},{1}};
eqlhsPd = matrix {{-1,-1,-1,-1,-1,-1}};
eqrhsPd = matrix {{-3}};
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 11/20)
assert(volume Pd == 11/20)
LE = reverse {1,37/10,25/4,23/4,11/4,11/20};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test low_dimensional_polytope/4.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1}};
raysP = map(QQ^1, QQ^0, 0);
linealityP = map(QQ^1, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0}};
ineqrhsPd = matrix {{1}};
eqlhsPd = matrix {{1}};
eqrhsPd = matrix {{1}};
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

-- Test low_dimensional_polytope/2.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1}};
raysP = map(QQ^1, QQ^0, 0);
linealityP = map(QQ^1, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0}};
ineqrhsPd = matrix {{1}};
eqlhsPd = matrix {{1}};
eqrhsPd = matrix {{1}};
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

-- Test cubical_h_vector/1.poly
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

-- Test cubical_h_vector/2.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,-1,0,0,0,0,0,0,0,0},{0,0,1,-1,0,0,0,0,0,0},{0,0,0,0,1,-1,0,0,0,0},{0,0,0,0,0,0,1,-1,0,0},{0,0,0,0,0,0,0,0,1,-1}};
raysP = map(QQ^5, QQ^0, 0);
linealityP = map(QQ^5, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,1,1,1,1},{-1,1,1,1,1},{1,-1,1,1,1},{-1,-1,1,1,1},{1,1,-1,1,1},{-1,1,-1,1,1},{1,-1,-1,1,1},{-1,-1,-1,1,1},{1,1,1,-1,1},{-1,1,1,-1,1},{1,-1,1,-1,1},{-1,-1,1,-1,1},{1,1,-1,-1,1},{-1,1,-1,-1,1},{1,-1,-1,-1,1},{-1,-1,-1,-1,1},{1,1,1,1,-1},{-1,1,1,1,-1},{1,-1,1,1,-1},{-1,-1,1,1,-1},{1,1,-1,1,-1},{-1,1,-1,1,-1},{1,-1,-1,1,-1},{-1,-1,-1,1,-1},{1,1,1,-1,-1},{-1,1,1,-1,-1},{1,-1,1,-1,-1},{-1,-1,1,-1,-1},{1,1,-1,-1,-1},{-1,1,-1,-1,-1},{1,-1,-1,-1,-1},{-1,-1,-1,-1,-1}};
ineqrhsPd = matrix {{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1}};
eqlhsPd = map(QQ^0, QQ^5, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 4/15)
assert(volume Pd == 4/15)
LE = reverse {1,46/15,10/3,8/3,2/3,4/15};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test wedge/1.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{2,-2,0,0,0,0,2,0,0},{0,0,2,-2,0,0,0,-2,0},{0,0,0,0,2,-2,0,0,2},{0,0,0,0,0,0,2,2,2}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1/2,1/2,-1/2,1},{-1,1,1,0},{-1,-1,1,0},{-1,-1,-1,0},{1,-1,1,0},{1,-1,-1,0},{1,1,1,0},{1,1,-1,0},{0,0,0,-1}};
ineqrhsPd = matrix {{1},{2},{2},{2},{2},{2},{2},{2},{0}};
eqlhsPd = map(QQ^0, QQ^4, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 32/3)
assert(volume Pd == 32/3)
LE = reverse {1,6,40/3,16,32/3};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test wedge/2-in.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{2,-2,0,0,0,0},{0,0,2,-2,0,0},{0,0,0,0,2,-2}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,1,-1},{-1,1,1},{-1,-1,1},{-1,-1,-1},{1,-1,1},{1,-1,-1},{1,1,1},{1,1,-1}};
ineqrhsPd = matrix {{2},{2},{2},{2},{2},{2},{2},{2}};
eqlhsPd = map(QQ^0, QQ^3, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 32/3)
assert(volume Pd == 32/3)
LE = reverse {1,16/3,8,32/3};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test wedge/4.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,0,0,1,0,0,1,0},{0,1,0,0,1,0,0,1},{0,0,1,0,0,1,0,0},{0,1,1,0,0,0,0,1},{1,0,0,0,0,1,1,0},{0,0,0,1,1,0,0,0},{0,0,0,0,1,1,0,0},{0,0,1,1,0,0,0,0},{1,1,0,0,0,0,1,1},{0,0,0,0,0,0,3,3}};
raysP = map(QQ^10, QQ^0, 0);
linealityP = map(QQ^10, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1/3,-1/3,2/3,-1/3,-1/3,2/3,2/3,2/3,-4/3,1},{-4,2,2,2,-1,-1,2,-1,-1,0},{-1,2,-1,2,-4,2,-1,2,-1,0},{1,-1/2,-1/2,1,-1/2,-1/2,-2,1,1,0},{1,1,-2,-1/2,-1/2,1,-1/2,-1/2,1,0},{-1/2,-1/2,1,1,1,-2,-1/2,-1/2,1,0},{-1/2,1,-1/2,-1/2,1,-1/2,1,-2,1,0},{2,-4,2,-1,2,-1,-1,2,-1,0},{2,-1,-1,-4,2,2,2,-1,-1,0},{0,0,0,0,0,0,0,0,0,-1}};
ineqrhsPd = matrix {{1},{3},{3},{3/2},{3/2},{3/2},{3/2},{3},{3},{0}};
eqlhsPd = matrix {{-1,-1,-1,0,0,0,0,0,0,0},{0,0,0,-1,-1,-1,0,0,0,0},{-2/3,1/3,1/3,-2/3,1/3,1/3,-1,0,0,0},{1/7,-4/7,3/7,1/7,-4/7,3/7,-2/7,-1,0,0},{1/5,1/5,-2/5,1/5,1/5,-2/5,-2/5,-2/5,-1,0}};
eqrhsPd = matrix {{-1},{-1},{-1/3},{-3/7},{-3/5}};
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 1/8)
assert(volume Pd == 1/8)
LE = reverse {1,13/4,33/8,21/8,7/8,1/8};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test wedge/1-in.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{2,-2,0,0,0,0},{0,0,2,-2,0,0},{0,0,0,0,2,-2}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,1,-1},{-1,1,1},{-1,-1,1},{-1,-1,-1},{1,-1,1},{1,-1,-1},{1,1,1},{1,1,-1}};
ineqrhsPd = matrix {{2},{2},{2},{2},{2},{2},{2},{2}};
eqlhsPd = map(QQ^0, QQ^3, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 32/3)
assert(volume Pd == 32/3)
LE = reverse {1,16/3,8,32/3};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test wedge/2.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{2,-2,0,0,0,0,2,0,0},{0,0,2,-2,0,0,0,-2,0},{0,0,0,0,2,-2,0,0,2},{6,0,0,6,6,0,-4,-4,-4}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,1,-1,-1},{-1,1,1,0},{-1,-1,1,0},{-1,-1,-1,0},{1,-1,1,0},{1,-1,-1,0},{1,1,1,0},{1,1,-1,0},{-3/2,3/2,-3/2,1}};
ineqrhsPd = matrix {{2},{2},{2},{2},{2},{2},{2},{2},{3}};
eqlhsPd = map(QQ^0, QQ^4, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 160/3)
assert(volume Pd == 160/3)
LE = reverse {1,10,104/3,48,160/3};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test wedge/3.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{2,-2,0,0,0,0,2,0,0},{0,0,2,-2,0,0,0,-2,0},{0,0,0,0,2,-2,0,0,2},{4,0,0,4,4,0,-4,-4,-4}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,1,-1,-1},{-1,1,1,0},{-1,-1,1,0},{-1,-1,-1,0},{1,-1,1,0},{1,-1,-1,0},{1,1,1,0},{1,1,-1,0},{-1,1,-1,1}};
ineqrhsPd = matrix {{2},{2},{2},{2},{2},{2},{2},{2},{2}};
eqlhsPd = map(QQ^0, QQ^4, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 128/3)
assert(volume Pd == 128/3)
LE = reverse {1,28/3,88/3,128/3,128/3};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test wedge/4-in.poly
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

-- Test BALANCED/6.poly
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

-- Test BALANCED/3.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,-1,-1,0,0},{0,1,-1,0,0},{0,0,0,1,-1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,0,1},{-1,0,-1},{1,-2,1},{1,-2,-1},{1,2,1},{1,2,-1}};
ineqrhsPd = matrix {{1},{1},{1},{1},{1},{1}};
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

-- Test steiner_points/2.poly
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

-- Test tensor/1-A.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,4},{2,5},{3,6}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,0,0},{-1,0,0}};
ineqrhsPd = matrix {{4},{-1}};
eqlhsPd = matrix {{1,-1,0},{1/2,1/2,-1}};
eqrhsPd = matrix {{-1},{-3/2}};
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

-- Test tensor/1.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{10,50,40,200},{20,60,80,240},{20,100,50,250},{40,120,100,300},{30,150,60,300},{60,180,120,360}};
raysP = map(QQ^6, QQ^0, 0);
linealityP = map(QQ^6, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-5/2,5/4,1,0,0,0},{7,-5,-1,0,0,0},{6,-1,-4,0,0,0},{-3,1,1,0,0,0}};
ineqrhsPd = matrix {{50},{-50},{-40},{10}};
eqlhsPd = matrix {{-1,1,1,-1,0,0},{-1/4,-3/4,5/4,3/4,-1,0},{-3/5,1/5,0,4/5,3/5,-1}};
eqrhsPd = matrix {{-10},{15/2},{-12}};
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 24000)
assert(volume Pd == 24000)
LE = reverse {1,30,1200,24000};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test tensor/1-B.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{10,30,50},{20,40,60}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,0},{1,0}};
ineqrhsPd = matrix {{-10},{50}};
eqlhsPd = matrix {{1,-1}};
eqrhsPd = matrix {{-10}};
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 40)
assert(volume Pd == 40)
LE = reverse {1,40};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test F_VECTOR/6.poly
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

-- Test F_VECTOR/1.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1},{-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1},{-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1}};
raysP = map(QQ^5, QQ^0, 0);
linealityP = map(QQ^5, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,0,0,0,-1},{-1,0,0,0,0},{0,-1,0,0,0},{0,0,-1,0,0},{0,0,0,-1,0},{0,0,0,0,1},{0,0,0,1,0},{0,0,1,0,0},{0,1,0,0,0},{1,0,0,0,0}};
ineqrhsPd = matrix {{1},{1},{1},{1},{1},{1},{1},{1},{1},{1}};
eqlhsPd = map(QQ^0, QQ^5, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 32)
assert(volume Pd == 32)
LE = reverse {1,10,40,80,80,32};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test F_VECTOR/2.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{-1,0,-1,-1,-1,-1,-1,-1},{-1,-1,0,-1,-1,-1,-1,-1},{-1,-1,-1,0,-1,-1,-1,-1},{-1,-1,-1,-1,0,-1,-1,-1},{-1,-1,-1,-1,-1,0,-1,-1},{-1,-1,-1,-1,-1,-1,0,-1},{-1,-1,-1,-1,-1,-1,-1,0}};
raysP = map(QQ^7, QQ^0, 0);
linealityP = map(QQ^7, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,1,1,1,1,1,1},{-1,0,0,0,0,0,0},{0,-1,0,0,0,0,0},{0,0,-1,0,0,0,0},{0,0,0,-1,0,0,0},{0,0,0,0,-1,0,0},{0,0,0,0,0,-1,0},{0,0,0,0,0,0,-1}};
ineqrhsPd = matrix {{-6},{1},{1},{1},{1},{1},{1},{1}};
eqlhsPd = map(QQ^0, QQ^7, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 1/5040)
assert(volume Pd == 1/5040)
LE = reverse {1,363/140,469/180,967/720,7/18,23/360,1/180,1/5040};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test F_VECTOR/5.poly
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

-- Test F_VECTOR/3.poly
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

-- Test lattice_properties/tpol2.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{-1,1,1,0,-1},{-1,-1,1,1,0}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,-1},{-1,1},{-1,0},{0,1},{1,0}};
ineqrhsPd = matrix {{1},{1},{1},{1},{1}};
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

-- Test lattice_properties/codim.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,2,0,0},{0,0,2,0},{0,0,0,2},{0,0,0,0}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,0,0,0},{0,-1,0,0},{0,0,-1,0},{1,1,1,0}};
ineqrhsPd = matrix {{0},{0},{0},{2}};
eqlhsPd = matrix {{0,0,0,-1}};
eqrhsPd = matrix {{0}};
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 4/3)
assert(volume Pd == 4/3)
LE = reverse {1,11/3,4,4/3};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test lattice_properties/cubesimplex.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,1,0,1},{0,1,1,0},{0,0,1,1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,1,1},{-1,1,-1},{1,-1,-1},{-1,-1,1}};
ineqrhsPd = matrix {{2},{0},{0},{0}};
eqlhsPd = map(QQ^0, QQ^3, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 1/3)
assert(volume Pd == 1/3)
LE = reverse {1,5/3,1,1/3};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test lattice_properties/smooth2.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{2,2,1,0},{0,2,2,0},{1,4,4,1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-2,1,0},{0,-1,0},{1,0,0},{0,1,0}};
ineqrhsPd = matrix {{0},{0},{2},{2}};
eqlhsPd = matrix {{0,3/2,-1}};
eqrhsPd = matrix {{-1}};
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 3/2)
assert(volume Pd == 3/2)
LE = reverse {1,5/2,3/2};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test lattice_properties/ps01.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{2,1,2,3},{3,2,1,3}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,-1},{-1,1},{0,1},{2,-1}};
ineqrhsPd = matrix {{-3},{1},{3},{3}};
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

-- Test lattice_properties/norm.poly
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

-- Test lattice_properties/compressed.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,1,1,2},{0,0,1,1}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,-1},{-1,1},{0,1},{1,-1}};
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

-- Test lattice_properties/simplex3.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,1,0,0},{0,0,1,0},{0,0,0,1}};
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

-- Test lattice_properties/smooth1.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{2,2,1,0},{0,2,2,0},{1,3,3,1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-2,1,0},{0,-1,0},{1,0,0},{0,1,0}};
ineqrhsPd = matrix {{0},{0},{2},{2}};
eqlhsPd = matrix {{0,1,-1}};
eqrhsPd = matrix {{-1}};
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

-- Test lattice_properties/tpol1.poly
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

-- Test lattice_properties/ps02.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,2,3},{2,1,4}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{3,-1},{-1,1},{-1,-1}};
ineqrhsPd = matrix {{5},{1},{-3}};
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

-- Test lattice_properties/cube3.poly
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

-- Test lattice_properties/ps03.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{2,1,2,3},{3,2,1,3},{3,0,3,6}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,-1,0},{-1,1,0},{0,1,0},{2,-1,0}};
ineqrhsPd = matrix {{-3},{1},{3},{3}};
eqlhsPd = matrix {{3,0,-1}};
eqrhsPd = matrix {{3}};
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

-- Test stable_set/1.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,0,0,0,0,0,0,0,0,1},{0,1,0,0,1,0,1,0,1,0},{0,1,1,0,0,0,0,1,1,0},{0,0,0,0,0,1,1,1,1,1}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,1,0,0},{1,0,1,0},{-1,0,0,0},{0,-1,0,0},{0,0,-1,0},{0,0,0,-1},{0,0,0,1}};
ineqrhsPd = matrix {{1},{1},{0},{0},{0},{0},{1}};
eqlhsPd = map(QQ^0, QQ^4, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 1/3)
assert(volume Pd == 1/3)
LE = reverse {1,19/6,11/3,11/6,1/3};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test stable_set/4.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,0,1,0,1,0,0,1,0,1,0,1,0,1,0,1},{1,1,0,0,0,0,1,1,0,0,1,1,0,0,1,1},{1,1,1,1,0,0,0,0,0,0,0,0,1,1,1,1},{0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,0,0,0},{0,-1,0,0},{0,0,-1,0},{0,0,0,-1},{1,0,0,0},{0,1,0,0},{0,0,1,0},{0,0,0,1}};
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

-- Test stable_set/2.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,0,1,0,0,1,0},{1,0,0,0,0,0,1},{0,0,0,1,0,0,1},{0,0,0,0,1,1,0}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,1,0,0},{1,0,1,0},{0,1,0,1},{0,0,1,1},{-1,0,0,0},{0,-1,0,0},{0,0,-1,0},{0,0,0,-1}};
ineqrhsPd = matrix {{1},{1},{1},{1},{0},{0},{0},{0}};
eqlhsPd = map(QQ^0, QQ^4, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 1/6)
assert(volume Pd == 1/6)
LE = reverse {1,5/2,7/3,1,1/6};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test squared_edge_lengths/1.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,1,0,0},{0,0,1,0},{0,0,0,1}};
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

-- Test join_polytopes/1.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{-1,1,-1,1,0,0,0,0},{-1,-1,1,1,0,0,0,0},{-1,-1,-1,-1,1,1,1,1},{0,0,0,0,-1,1,-1,1},{0,0,0,0,-1,-1,1,1}};
raysP = map(QQ^5, QQ^0, 0);
linealityP = map(QQ^5, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-2,0,1,0,0},{2,0,1,0,0},{0,-2,1,0,0},{0,2,1,0,0},{0,0,-1/2,-1,0},{0,0,-1/2,1,0},{0,0,-1/2,0,-1},{0,0,-1/2,0,1}};
ineqrhsPd = matrix {{1},{1},{1},{1},{1/2},{1/2},{1/2},{1/2}};
eqlhsPd = map(QQ^0, QQ^5, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 16/15)
assert(volume Pd == 16/15)
LE = reverse {1,18/5,16/3,16/3,8/3,16/15};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test join_polytopes/2-in.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,1,0,0},{0,0,1,0},{0,0,0,1}};
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

-- Test join_polytopes/1-in.poly
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

-- Test join_polytopes/2.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{-1,1,-1,1,0,0,0,0},{-1,-1,1,1,0,0,0,0},{-1,-1,-1,-1,1,1,1,1},{0,0,0,0,0,1,0,0},{0,0,0,0,0,0,1,0},{0,0,0,0,0,0,0,1}};
raysP = map(QQ^6, QQ^0, 0);
linealityP = map(QQ^6, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-2,0,1,0,0,0},{0,-2,1,0,0,0},{2,0,1,0,0,0},{0,2,1,0,0,0},{0,0,-1,2,2,2},{0,0,0,-1,0,0},{0,0,0,0,-1,0},{0,0,0,0,0,-1}};
ineqrhsPd = matrix {{1},{1},{1},{1},{1},{0},{0},{0}};
eqlhsPd = map(QQ^0, QQ^6, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 1/45)
assert(volume Pd == 1/45)
LE = reverse {1,17/5,841/180,10/3,47/36,4/15,1/45};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test join_polytopes/3.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{-1,1,-1,1,0,0,0,0},{-1,-1,1,1,0,0,0,0},{0,0,0,0,-1,1,-1,1},{0,0,0,0,-1,-1,1,1}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,0,-1,0},{-1,0,1,0},{-1,0,0,-1},{-1,0,0,1},{1,0,-1,0},{1,0,1,0},{1,0,0,-1},{1,0,0,1},{0,-1,-1,0},{0,-1,1,0},{0,-1,0,-1},{0,-1,0,1},{0,1,-1,0},{0,1,1,0},{0,1,0,-1},{0,1,0,1}};
ineqrhsPd = matrix {{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1}};
eqlhsPd = map(QQ^0, QQ^4, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 8/3)
assert(volume Pd == 8/3)
LE = reverse {1,8/3,16/3,16/3,8/3};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test ONE_VERTEX/6.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,1,0,1,0,0},{1,0,0,0,0,1},{0,0,1,0,1,0},{0,0,1,0,0,1},{0,0,0,1,1,0},{1,1,0,0,0,0},{1,0,0,0,1,0},{0,1,1,0,0,0},{0,0,0,1,0,1}};
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

-- Test ONE_VERTEX/1.poly
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

-- Test ONE_VERTEX/4.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,1},{1,0}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,0},{-1,0}};
ineqrhsPd = matrix {{1},{0}};
eqlhsPd = matrix {{-1,-1}};
eqrhsPd = matrix {{-1}};
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

-- Test ONE_VERTEX/2.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,1},{1,0}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,0},{-1,0}};
ineqrhsPd = matrix {{1},{0}};
eqlhsPd = matrix {{-1,-1}};
eqrhsPd = matrix {{-1}};
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

-- Test conv/1-A.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,2,3},{1,3,4}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,1},{3,-2},{-2,1}};
ineqrhsPd = matrix {{1},{1},{-1}};
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

-- Test conv/1.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,2,100,200,300},{1,3,100,300,400}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-3/2,1},{1,-1},{-2,1},{3/2,-1},{-1,1}};
ineqrhsPd = matrix {{0},{0},{-1},{50},{100}};
eqlhsPd = map(QQ^0, QQ^2, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 19999/2)
assert(volume Pd == 19999/2)
LE = reverse {1,399/2,19999/2};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test conv/1-B.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{10,20,30},{10,30,40}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,1},{3/2,-1},{-2,1}};
ineqrhsPd = matrix {{10},{5},{-10}};
eqlhsPd = map(QQ^0, QQ^2, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 50)
assert(volume Pd == 50)
LE = reverse {1,15,50};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test conv/1-C.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{100,200,300},{100,300,400}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,1},{3/2,-1},{-2,1}};
ineqrhsPd = matrix {{100},{50},{-100}};
eqlhsPd = map(QQ^0, QQ^2, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 5000)
assert(volume Pd == 5000)
LE = reverse {1,150,5000};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test VISUAL/in1.poly
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

-- Test VISUAL/6.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{3,2,0,0,2},{2,3,2,0,0}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,2},{-1,0},{0,-1},{2,-1},{1,1}};
ineqrhsPd = matrix {{4},{0},{0},{4},{5}};
eqlhsPd = map(QQ^0, QQ^2, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 13/2)
assert(volume Pd == 13/2)
LE = reverse {1,7/2,13/2};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test VISUAL/8.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{-1,1}};
raysP = map(QQ^1, QQ^0, 0);
linealityP = map(QQ^1, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1},{1}};
ineqrhsPd = matrix {{1},{1}};
eqlhsPd = map(QQ^0, QQ^1, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 2)
assert(volume Pd == 2)
LE = reverse {1,2};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test VISUAL/1.poly
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

-- Test VISUAL/simplex.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,1,0,0},{0,0,1,0},{0,0,0,1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,0,0},{0,-1,0},{0,0,-1},{1,1,1}};
ineqrhsPd = matrix {{0},{0},{0},{1}};
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

-- Test VISUAL/9.poly
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

-- Test VISUAL/regular_24_cell.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,0,0,0,0,1,-1,1,0,1,0,1,0,-1,-1,0,-1,1,-1,0,0,0,0,-1},{0,1,0,0,-1,1,0,0,1,0,1,-1,-1,1,0,-1,0,0,-1,1,0,0,-1,0},{0,0,1,-1,0,0,0,1,1,-1,-1,0,1,0,1,-1,-1,0,0,0,1,-1,0,0},{-1,-1,-1,-1,-1,0,-1,0,0,0,0,0,0,0,0,0,0,1,0,1,1,1,1,1}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,1,-1,-1},{1,0,0,0},{1,1,1,-1},{1,1,1,1},{0,1,0,0},{1,1,-1,1},{1,-1,1,1},{-1,1,-1,-1},{-1,-1,1,1},{-1,-1,-1,-1},{0,0,0,1},{-1,-1,-1,1},{-1,-1,1,-1},{1,-1,-1,-1},{0,-1,0,0},{0,0,1,0},{0,0,-1,0},{-1,1,1,1},{-1,1,1,-1},{0,0,0,-1},{1,-1,1,-1},{-1,1,-1,1},{1,-1,-1,1},{-1,0,0,0}};
ineqrhsPd = matrix {{2},{1},{2},{2},{1},{2},{2},{2},{2},{2},{1},{2},{2},{2},{1},{1},{1},{2},{2},{1},{2},{2},{2},{1}};
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

-- Test VISUAL/3.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,1},{0,1},{0,1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,0,0},{-1,0,0}};
ineqrhsPd = matrix {{1},{0}};
eqlhsPd = matrix {{1,-1,0},{1/2,1/2,-1}};
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

-- Test VISUAL/7.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,2,1,2},{1,2,3,4}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-2,1},{1,-2},{1,0},{-1,1}};
ineqrhsPd = matrix {{1},{-2},{2},{2}};
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

-- Test VISUAL/in2.poly
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

-- Test DIHEDRAL_ANGLES/c.poly
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

-- Test GALE/1.poly
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

-- Test GALE/3.poly
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

-- Test VISUAL_STEINER/1.poly
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

-- Test CENTRALLY_SYMMETRIC/1.poly
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

-- Test face_lattice/1.poly
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

-- Test face_lattice/2.poly
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

-- Test SIMPLICIAL/1.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,1,0,0},{1,0,0,0},{0,0,1,0}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,0,0},{0,-1,0},{0,0,-1},{1,1,1}};
ineqrhsPd = matrix {{0},{0},{0},{1}};
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

-- Test SIMPLICIAL/2.poly
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

-- Test 2-face-sizes/1.poly
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

-- Test 2-face-sizes/2.poly
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

-- Test gkz_vector/1.poly
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

-- Test common_refinement/1.poly
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

-- Test common_refinement/2-in.poly
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

-- Test common_refinement/1a-in.poly
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

-- Test common_refinement/1-in.poly
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

-- Test common_refinement/2a-in.poly
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

-- Test common_refinement/2.poly
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

-- Test regular_subdivision/2p.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,1,-1,-1,2,-2},{1,-1,1,-1,0,0}};
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

-- Test lattice_pyramid/simpyr2.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,3,0,0},{0,0,3,0},{0,0,0,1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,1,3},{-1,0,0},{0,-1,0},{0,0,-1}};
ineqrhsPd = matrix {{3},{0},{0},{0}};
eqlhsPd = map(QQ^0, QQ^3, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 3/2)
assert(volume Pd == 3/2)
LE = reverse {1,4,9/2,3/2};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test lattice_pyramid/cubepyr1.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{-1,1,-1,1,-1,1,-1,1,-1},{-1,-1,1,1,-1,-1,1,1,-1},{-1,-1,-1,-1,1,1,1,1,-1},{0,0,0,0,0,0,0,0,3}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,0,0,0},{3/2,0,0,1},{0,-1,0,0},{0,3/2,0,1},{0,0,-1,0},{0,0,3/2,1},{0,0,0,-1}};
ineqrhsPd = matrix {{1},{3/2},{1},{3/2},{1},{3/2},{0}};
eqlhsPd = map(QQ^0, QQ^4, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 6)
assert(volume Pd == 6)
LE = reverse {1,6,12,12,6};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test lattice_pyramid/simplex.poly
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

-- Test lattice_pyramid/cube.poly
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

-- Test lattice_pyramid/simpyr1.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,3,0,1},{0,0,3,1},{0,0,0,2}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{2,2,1},{0,0,-1},{0,-2,1},{-2,0,1}};
ineqrhsPd = matrix {{6},{0},{0},{0}};
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

-- Test totally_dual_integral/1.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,1,-1,-1},{-1,1,1,-1}};
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

-- Test integer_points_projection/birkhoff.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,0,0,1,0,0,0,0,1,0,0,1,1,0,0,1,0,0,0,0,0,0,0,0},{0,1,0,0,1,0,0,1,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,1},{0,0,1,0,0,1,0,0,0,0,0,0,0,1,0,0,1,0,0,1,0,0,1,0},{0,0,0,0,0,0,1,0,0,1,0,0,0,0,1,0,0,1,1,0,0,1,0,0},{0,1,1,0,0,0,0,0,0,1,1,0,0,1,1,0,0,0,0,0,0,0,0,0},{1,0,0,0,0,1,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,1,0},{0,0,0,1,1,0,0,0,0,0,0,0,1,0,0,0,0,1,1,0,0,0,0,1},{0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,1,1,0,0,0},{0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0},{0,0,1,1,0,0,0,0,1,1,0,0,0,0,0,0,0,0,1,1,0,0,0,0},{1,1,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,1,1,0,0},{0,0,0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,1,1},{0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1},{0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,0,0},{0,0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0},{1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}};
raysP = map(QQ^16, QQ^0, 0);
linealityP = map(QQ^16, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,0,0,0,0,0,0,0,0,0,-1,0,0,0,0,0},{-1,-1,-1,0,-1,-1,-1,0,-1,-1,-1,0,0,0,0,0},{-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{0,0,0,0,0,-1,0,0,0,0,0,0,0,0,0,0},{0,0,1,0,0,0,1,0,0,0,1,0,0,0,0,0},{0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0},{0,0,0,0,0,0,0,0,0,-1,0,0,0,0,0,0},{0,0,0,0,0,0,-1,0,0,0,0,0,0,0,0,0},{0,1,0,0,0,1,0,0,0,1,0,0,0,0,0,0},{0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0},{0,-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{0,0,0,0,-1,0,0,0,0,0,0,0,0,0,0,0},{0,0,0,0,0,0,0,0,-1,0,0,0,0,0,0,0},{0,0,-1,0,0,0,0,0,0,0,0,0,0,0,0,0},{1,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0},{1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0}};
ineqrhsPd = matrix {{0},{-2},{0},{0},{1},{1},{0},{0},{1},{1},{0},{0},{0},{0},{1},{1}};
eqlhsPd = matrix {{-1,-1,-1,-1,0,0,0,0,0,0,0,0,0,0,0,0},{0,0,0,0,-1,-1,-1,-1,0,0,0,0,0,0,0,0},{0,0,0,0,0,0,0,0,-1,-1,-1,-1,0,0,0,0},{-3/4,1/4,1/4,1/4,-3/4,1/4,1/4,1/4,-3/4,1/4,1/4,1/4,-1,0,0,0},{1/13,-9/13,4/13,4/13,1/13,-9/13,4/13,4/13,1/13,-9/13,4/13,4/13,-3/13,-1,0,0},{1/10,1/10,-3/5,2/5,1/10,1/10,-3/5,2/5,1/10,1/10,-3/5,2/5,-3/10,-3/10,-1,0},{1/7,1/7,1/7,-3/7,1/7,1/7,1/7,-3/7,1/7,1/7,1/7,-3/7,-3/7,-3/7,-3/7,-1}};
eqrhsPd = matrix {{-1},{-1},{-1},{-1/4},{-4/13},{-2/5},{-4/7}};
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 11/11340)
assert(volume Pd == 11/11340)
LE = reverse {1,65/18,379/63,35117/5670,43/10,1109/540,2/3,19/135,11/630,11/11340};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test integer_points_projection/cutsquare.poly
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

-- Test integer_points_projection/nonnormal.poly
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

-- Test automorphisms/3.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,-1,-3,-1,0,-4,-2,-4,-1,0,0,-1,0,0,-2,-4,-4,-2,-2,-4},{-3,-4,-4,-2,-2,0,2,-4,2,2,-3,-4,2,0,2,0,-4,-4,-2,-2},{1,0,0,2,2,-1,1,-1,2,2,-4,-5,-2,-4,-2,-4,-6,-6,-6,-6}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,1,-1},{0,-1,1},{-1,0,0},{0,0,-1},{-1,1,0},{0,-1,0},{-1,0,1},{1,-1,0},{1,0,-1},{0,0,1},{0,1,0},{1,0,0}};
ineqrhsPd = matrix {{4},{4},{4},{6},{4},{4},{3},{3},{4},{2},{2},{0}};
eqlhsPd = map(QQ^0, QQ^3, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 352/3)
assert(volume Pd == 352/3)
LE = reverse {1,38/3,62,352/3};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test VISUAL_TRIANGULATION/1.poly
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

-- Test transformation/cube.poly
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

-- Test orthantify/2-in.poly
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

-- Test orthantify/2.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,2,0,2,0,2,0,2},{0,0,2,2,0,0,2,2},{0,0,0,0,2,2,2,2}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,0,0},{1,0,0},{0,-1,0},{0,1,0},{0,0,-1},{0,0,1}};
ineqrhsPd = matrix {{0},{2},{0},{2},{0},{2}};
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

-- Test SCHLEGEL_STEINER/1.poly
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

-- Test subcone/s2.poly
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

-- Test schlegel_vertices/1.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,-1,0,0,0,0},{0,0,1,-1,0,0},{0,0,0,0,1,-1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,1,-1},{-1,1,1},{-1,-1,1},{-1,-1,-1},{1,-1,1},{1,-1,-1},{1,1,1},{1,1,-1}};
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

-- Test remove/1.poly
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

-- Test CENTERED/1.poly
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

-- Test CENTERED/2.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,0,1,0,1,0,0,1,0,1,0,1,0,1,0,1},{1,1,0,0,0,0,1,1,0,0,1,1,0,0,1,1},{1,1,1,1,0,0,0,0,0,0,0,0,1,1,1,1},{0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,0,0,0},{1,0,0,0},{0,-1,0,0},{0,1,0,0},{0,0,-1,0},{0,0,1,0},{0,0,0,-1},{0,0,0,1}};
ineqrhsPd = matrix {{0},{1},{0},{1},{0},{1},{0},{1}};
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

-- Test toric_g_vector/1.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1},{-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1},{-1,-1,-1,-1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1},{-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1},{-1,1,-1,1,-1,1,-1,1,-1,1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1}};
raysP = map(QQ^5, QQ^0, 0);
linealityP = map(QQ^5, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,0,0,0,0},{1,0,0,0,0},{0,-1,0,0,0},{0,1,0,0,0},{0,0,-1,0,0},{0,0,1,0,0},{0,0,0,-1,0},{0,0,0,1,0},{0,0,0,0,-1},{0,0,0,0,1}};
ineqrhsPd = matrix {{1},{1},{1},{1},{1},{1},{1},{1},{1},{1}};
eqlhsPd = map(QQ^0, QQ^5, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 32)
assert(volume Pd == 32)
LE = reverse {1,10,40,80,80,32};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test toric_g_vector/2.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,1,0,0,0,0,0,0},{0,0,1,0,0,0,0,0},{0,0,0,1,0,0,0,0},{0,0,0,0,1,0,0,0},{0,0,0,0,0,1,0,0},{0,0,0,0,0,0,1,0},{0,0,0,0,0,0,0,1}};
raysP = map(QQ^7, QQ^0, 0);
linealityP = map(QQ^7, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,1,1,1,1,1,1},{-1,0,0,0,0,0,0},{0,-1,0,0,0,0,0},{0,0,-1,0,0,0,0},{0,0,0,-1,0,0,0},{0,0,0,0,-1,0,0},{0,0,0,0,0,-1,0},{0,0,0,0,0,0,-1}};
ineqrhsPd = matrix {{1},{0},{0},{0},{0},{0},{0},{0}};
eqlhsPd = map(QQ^0, QQ^7, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 1/5040)
assert(volume Pd == 1/5040)
LE = reverse {1,363/140,469/180,967/720,7/18,23/360,1/180,1/5040};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test toric_g_vector/3.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,1,1,1,1,0,0,0,0,0,0,0,0,0,0},{1,0,0,0,0,1,1,1,1,0,0,0,0,0,0},{0,1,0,0,0,1,0,0,0,1,1,1,0,0,0},{0,0,1,0,0,0,1,0,0,1,0,0,1,1,0},{0,0,0,1,0,0,0,1,0,0,1,0,1,0,1},{0,0,0,0,1,0,0,0,1,0,0,1,0,1,1}};
raysP = map(QQ^6, QQ^0, 0);
linealityP = map(QQ^6, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,-1,0,0,0,0},{0,0,1,0,0,0},{0,0,0,0,1,0},{-1,-1,-1,-1,-1,0},{-1,0,0,0,0,0},{0,0,0,1,0,0},{0,1,0,0,0,0},{1,0,0,0,0,0},{0,0,-1,0,0,0},{0,0,0,-1,0,0},{0,0,0,0,-1,0},{1,1,1,1,1,0}};
ineqrhsPd = matrix {{0},{1},{1},{-1},{0},{1},{1},{1},{0},{0},{0},{2}};
eqlhsPd = matrix {{-1,-1,-1,-1,-1,-1}};
eqrhsPd = matrix {{-2}};
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 13/60)
assert(volume Pd == 13/60)
LE = reverse {1,101/30,5,47/12,3/2,13/60};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test latte/zerodim.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0},{0}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,0}};
ineqrhsPd = matrix {{1}};
eqlhsPd = matrix {{-1,0},{0,-1}};
eqrhsPd = matrix {{0},{0}};
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

-- Test latte/ehrpol1.poly
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

-- Test latte/zero2.poly
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

-- Test latte/ehrpol3.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,1,0,1,1},{0,0,1,1,1},{0,0,0,30,17},{0,0,0,30,18}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{5/2,5/2,1,-13/12},{-5/2,0,-1,13/12},{0,-5/2,-1,13/12},{0,0,1,-1},{0,0,-18/17,1}};
ineqrhsPd = matrix {{5/2},{0},{0},{0},{0}};
eqlhsPd = map(QQ^0, QQ^4, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 5/4)
assert(volume Pd == 5/4)
LE = reverse {1,-1/3,1/4,17/6,5/4};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test latte/count.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,1,1,1,-1,-1,-1,-1},{-1,-1,1,1,1,1,-1,-1},{-1,1,1,-1,1,-1,1,-1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,0,0},{1,0,0},{0,-1,0},{0,1,0},{0,0,-1},{0,0,1},{0,0,0}};
ineqrhsPd = matrix {{1},{1},{1},{1},{1},{1},{1}};
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

-- Test latte/zero1.poly
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
assert(volume P == 1)
assert(volume Pd == 1)
LE = reverse {1};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test latte/ehrpol2.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,0,5,5},{0,0,1,5},{0,3,4,7}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,-15,20},{-1,1,0},{1,-5,0},{1,15/13,-20/13}};
ineqrhsPd = matrix {{60},{0},{0},{0}};
eqlhsPd = map(QQ^0, QQ^3, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 10)
assert(volume Pd == 10)
LE = reverse {1,0,5,10};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test latte/ineq.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,2,0},{0,2,1}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,-1},{2,-1},{-1,2}};
ineqrhsPd = matrix {{-1},{2},{2}};
eqlhsPd = map(QQ^0, QQ^2, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 3/2)
assert(volume Pd == 3/2)
LE = reverse {1,3/2,3/2};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test latte/sparse.poly
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

-- Test minimal_vertex_angle/1.poly
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

-- Test LATTICE/1.poly
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

-- Test VISUAL_LATTICE/in1.poly
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

-- Test VISUAL_LATTICE/cube2.poly
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

-- Test VISUAL_LATTICE/simplex2.poly
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

-- Test VISUAL_LATTICE/simplex3.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{0,4,0,0},{0,0,4,0},{0,0,0,4}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,1,1},{-1,0,0},{0,-1,0},{0,0,-1}};
ineqrhsPd = matrix {{4},{0},{0},{0}};
eqlhsPd = map(QQ^0, QQ^3, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 32/3)
assert(volume Pd == 32/3)
LE = reverse {1,22/3,16,32/3};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test poly2lp/4.poly
-- Checking ehrhart and volume
TEST ///
verticesP = map(QQ^15, QQ^0, 0);
raysP = map(QQ^15, QQ^0, 0);
linealityP = map(QQ^15, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = map(QQ^0, QQ^15, 0);
ineqrhsPd = map(QQ^0, QQ^1, 0);
eqlhsPd = map(QQ^0, QQ^15, 0);
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

-- Test poly2lp/3.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1},{-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1},{-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1},{-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1},{-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1},{-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1},{-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1}};
raysP = map(QQ^7, QQ^0, 0);
linealityP = map(QQ^7, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{0,0,0,-1,0,0,0},{0,0,0,0,-1,0,0},{0,0,0,0,0,1,0},{0,0,0,0,0,0,1},{0,0,0,0,0,0,-1},{-1,0,0,0,0,0,0},{0,0,0,1,0,0,0},{0,0,-1,0,0,0,0},{1,0,0,0,0,0,0},{0,-1,0,0,0,0,0},{0,0,1,0,0,0,0},{0,0,0,0,0,-1,0},{0,1,0,0,0,0,0},{0,0,0,0,1,0,0}};
ineqrhsPd = matrix {{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1}};
eqlhsPd = map(QQ^0, QQ^7, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 128)
assert(volume Pd == 128)
LE = reverse {1,14,84,280,560,672,448,128};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test transportation/1.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,1,0,0,0,0},{0,0,1,1,0,0},{0,0,0,0,1,1},{0,0,0,1,0,1},{0,1,0,0,1,0},{1,0,1,0,0,0},{0,0,1,0,1,0},{1,0,0,0,0,1},{0,1,0,1,0,0}};
raysP = map(QQ^9, QQ^0, 0);
linealityP = map(QQ^9, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,0,0,0,0,0,0,0,0},{0,-1,0,0,0,0,0,0,0},{0,0,-1,0,0,0,0,0,0},{0,0,0,-1,0,0,0,0,0},{0,0,0,0,-1,0,0,0,0},{0,0,0,0,0,-1,0,0,0},{0,0,0,0,0,0,-1,0,0},{0,0,0,0,0,0,0,-1,0},{0,0,0,0,0,0,0,0,-1}};
ineqrhsPd = matrix {{0},{0},{0},{0},{0},{0},{0},{0},{0}};
eqlhsPd = matrix {{-1,-1,-1,0,0,0,0,0,0},{0,0,0,-1,-1,-1,0,0,0},{0,0,0,0,0,0,-1,-1,-1},{-2/3,1/3,1/3,-2/3,1/3,1/3,-2/3,1/3,1/3},{0,-1/2,1/2,0,-1/2,1/2,0,-1/2,1/2}};
eqrhsPd = matrix {{-1},{-1},{-1},{0},{0}};
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

-- Test transportation/2.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{3,3,1,2,0,0},{0,0,2,1,3,3},{0,4,6,0,6,2},{6,2,0,6,0,4},{4,0,0,5,1,5},{1,5,5,0,4,0}};
raysP = map(QQ^6, QQ^0, 0);
linealityP = map(QQ^6, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,0,0,0,0,0},{0,-1,0,0,0,0},{0,0,-1,0,0,0},{0,0,0,-1,0,0},{0,0,0,0,-1,0},{0,0,0,0,0,-1}};
ineqrhsPd = matrix {{0},{0},{0},{0},{0},{0}};
eqlhsPd = matrix {{-1,-1,0,0,0,0},{0,0,-1,-1,0,0},{0,0,0,0,-1,-1},{-1/2,1/2,-1/2,1/2,-1/2,1/2}};
eqrhsPd = matrix {{-3},{-6},{-5},{0}};
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 14)
assert(volume Pd == 14)
LE = reverse {1,7,14};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test tight_span/2-in.poly
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

-- Test tight_span/1-in.poly
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

-- Test stack/2-in.poly
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

-- Test stack/5-in.poly
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

-- Test stack/1-in.poly
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

-- Test stack/3-in.poly
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

-- Test stack/7-in.poly
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

-- Test stack/4-in.poly
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

-- Test stack/6-in.poly
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

-- Test to_lp_client/6.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{-1,0,0,-1,0,0,1,1,1,0,0,-1},{0,-1,0,1,1,1,-1,0,0,0,-1,0},{1,1,1,0,-1,0,0,-1,0,-1,0,0},{0,0,-1,0,0,-1,0,0,-1,1,1,1}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,1,1,1},{1,-1,1,1},{-1,-1,1,1},{1,1,-1,1},{-1,1,-1,1},{1,-1,-1,1},{-1,-1,-1,1},{1,1,1,-1},{-1,1,1,-1},{1,-1,1,-1},{-1,-1,1,-1},{1,1,-1,-1},{-1,1,-1,-1},{1,-1,-1,-1}};
ineqrhsPd = matrix {{2},{2},{2},{2},{2},{2},{2},{2},{2},{2},{2},{2},{2},{2}};
eqlhsPd = matrix {{-1,-1,-1,-1}};
eqrhsPd = matrix {{0}};
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 10/3)
assert(volume Pd == 10/3)
LE = reverse {1,11/3,5,10/3};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test to_lp_client/1.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{1,2,2,0,0,0,2,0},{0,0,0,0,0,2,2,1},{3,2,0,0,3,0,0,3}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{1,1,1},{1,0,0},{0,0,1},{0,3,1},{-1,0,0},{0,-1,0},{0,0,-1}};
ineqrhsPd = matrix {{4},{2},{3},{6},{0},{0},{0}};
eqlhsPd = map(QQ^0, QQ^3, 0);
eqrhsPd = map(QQ^0, QQ^1, 0);
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 43/6)
assert(volume Pd == 43/6)
LE = reverse {1,29/6,10,43/6};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test to_lp_client/4.poly
-- Checking ehrhart and volume
TEST ///
verticesP = matrix {{2,-2},{-1,-1}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
P = convexHull(verticesP,raysP,linealityP);
ineqlhsPd = matrix {{-1,0},{1,0}};
ineqrhsPd = matrix {{2},{2}};
eqlhsPd = matrix {{0,-1}};
eqrhsPd = matrix {{1}};
Pd = intersection(ineqlhsPd, ineqrhsPd, eqlhsPd, eqrhsPd);
assert(volume P == 4)
assert(volume Pd == 4)
LE = reverse {1,4};
etest = vector apply(LE, e->promote(e, QQ));
ep = vector apply(flatten entries (coefficients ehrhart P)#1, e->lift(e, QQ));
epd = vector apply(flatten entries (coefficients ehrhart Pd)#1, e->lift(e, QQ));
assert(ep == etest)
assert(epd == etest)
///

-- Test to_lp_client/2.poly
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

