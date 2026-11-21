-- Families of unital binomial ideals over a given coefficient field K, for
-- the positive characteristic tests.  Variables are made with getSymbol so
-- that the test files keep their own x, y, z.

-- ideal of adjacent 2x2 minors of a generic m x n matrix
pchAdjacentMinors = (K, m, n) -> (
    x := getSymbol "x";
    R := K(monoid [x_(1,1)..x_(m,n)]);
    X := genericMatrix(R, n, m);
    ideal flatten for i from 0 to m-2 list for j from 0 to n-2 list
        X_(j,i)*X_(j+1,i+1) - X_(j+1,i)*X_(j,i+1));

-- commuting birth-and-death ideal on the grid {0..a} x {0..b}
pchBirthDeath = (K, a, b) -> (
    Rs := getSymbol "R"; Ls := getSymbol "L"; Us := getSymbol "U"; Ds := getSymbol "D";
    vs := flatten flatten for x from 0 to a list for y from 0 to b list (
        (if x < a then {Rs_(x,y)} else {}) | (if x > 0 then {Ls_(x,y)} else {}) |
        (if y < b then {Us_(x,y)} else {}) | (if y > 0 then {Ds_(x,y)} else {}));
    S := K(monoid [vs]);
    v := (s, x, y) -> S_(position(vs, w -> w === s_(x,y)));
    ideal flatten flatten for x from 0 to a-1 list for y from 0 to b-1 list {
        v(Us,x,y)*v(Rs,x,y+1) - v(Rs,x,y)*v(Us,x+1,y),
        v(Rs,x,y+1)*v(Ds,x+1,y+1) - v(Ds,x,y+1)*v(Rs,x,y),
        v(Ds,x+1,y+1)*v(Ls,x+1,y) - v(Ls,x+1,y+1)*v(Ds,x,y+1),
        v(Ls,x+1,y)*v(Us,x,y) - v(Us,x+1,y)*v(Ls,x+1,y+1)});

-- parity binomial edge ideal of the graph on 1..n with edge list E
-- (Kahle, Sarmiento, Windisch 2016): x_i x_j - y_i y_j for the edges
pchParityEdgeIdeal = (K, n, E) -> (
    x := getSymbol "x"; y := getSymbol "y";
    R := K(monoid [x_1..x_n, y_1..y_n]);
    ideal for e in E list R_(e#0-1)*R_(e#1-1) - R_(n+e#0-1)*R_(n+e#1-1));

-- binomial edge ideal of the graph on 1..n with edge list E
pchBinomialEdgeIdeal = (K, n, E) -> (
    x := getSymbol "x"; y := getSymbol "y";
    R := K(monoid [x_1..x_n, y_1..y_n]);
    ideal for e in E list R_(e#0-1)*R_(n+e#1-1) - R_(e#1-1)*R_(n+e#0-1));

pchCycleEdges = n -> for i from 1 to n list {i, i % n + 1};
pchPathEdges = n -> for i from 1 to n - 1 list {i, i + 1};
pchCompleteEdges = n -> flatten for i from 1 to n list for j from i + 1 to n list {i, j};

-- is the graph on 1..n with edges E bipartite?
pchIsBipartite = (n, E) -> (
    color := new MutableList from toList(n + 1 : null);
    ok := true;
    for s from 1 to n do if color#s === null then (
        color#s = 0;
        queue := {s};
        while #queue > 0 do (
            v := first queue; queue = drop(queue, 1);
            for e in E do (
                w := if e#0 == v then e#1 else if e#1 == v then e#0 else continue;
                if color#w === null then (color#w = 1 - color#v; queue = append(queue, w))
                else if color#w == color#v then ok = false)));
    ok);
