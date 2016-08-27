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
