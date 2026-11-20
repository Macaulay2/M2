-- doc-core-library.m2 -- independent tests written from the documentation
-- (node library: seven nonnegative forms, both methods) against the
-- literature: [Rez00] B. Reznick, Some concrete aspects of Hilbert's 17th
-- problem (pp. 7-11), [Sch16] C. Scheiderer, Sums of squares of polynomials
-- with rational coefficients (Thm. 2.1, Ex. 3.9), [Har99] W. R. Harris, Real
-- even symmetric ternary forms (p. 224 and Lemma 6.8).
-- Self-contained; assumes SumsOfSquares is loaded.
setRandomSeed 0;  -- fixed seed for anything random in the solver path

isPSDQQ = Q -> (
    n := numrows Q;
    if n != numcols Q or Q != transpose Q then return false;
    A := mutableMatrix Q;
    for k from 0 to n-1 do (
        p := A_(k,k);
        if p < 0 then return false;
        if p == 0 then (
            for j from k+1 to n-1 do if A_(k,j) != 0 then return false;
            ) else (
            for i from k+1 to n-1 do for j from k+1 to n-1 do
                A_(i,j) = A_(i,j) - A_(i,k)*A_(k,j)/p;
            );
        );
    true)
okStatus = "SDP solved, primal-dual feasible";
-- solveSOS does not claim that f is a sum of squares
noSOSclaim = f -> (
    sol := solveSOS f; Q := sol#GramMatrix; mon := sol#Monomials;
    sol#Status =!= okStatus and
      not (Q =!= null and mon =!= null and ring Q === QQ and isPSDQQ Q
           and (transpose(mon)*Q*mon - f == 0)));
-- number of distinct complex projective zeros of the singular locus of a
-- ternary form (every real zero of a nonnegative form is singular)
singDegree = f -> (
    S := ring f;
    degree radical saturate(ideal(f) + ideal jacobian ideal f, ideal gens S));
-- f and its gradient vanish at the point p (coordinates in some ring A)
singZero = (f, p) -> (
    S := ring f; A := ring p#0;
    phi := map(A, S, p);
    phi f == 0 and all(flatten entries jacobian ideal f, g -> phi g == 0));
-- projectively distinct points
projDistinct = L -> all(#L, i -> all(i, j ->
    any(subsets(#(L#i), 2), ij -> (L#i)#(ij#0)*(L#j)#(ij#1) - (L#i)#(ij#1)*(L#j)#(ij#0) != 0)));
-- the orbit of points under permutations and sign changes, up to scaling
orbit = base -> (
    pts := {};
    for b in base do for pm in permutations 3 do for sg in (0..7) do (
        q := apply(3, i -> (if (sg >> i) % 2 == 1 then -1 else 1) * b#(pm#i));
        if all(pts, p -> projDistinct {p, q}) then pts = append(pts, q);
        );
    pts);

-------------------------------------------------------------------------
-- formulas
R = QQ[x,y,z];
M = library("Motzkin", R);
Rob = library("Robinson", R);
Sch = library("Schmuedgen", R);
Sc = library("Scheiderer", R);
H = library("Harris", R);
-- [Rez00] p. 7: M = x^4y^2 + x^2y^4 + z^6 - 3x^2y^2z^2
assert(M == x^4*y^2 + x^2*y^4 + z^6 - 3*x^2*y^2*z^2);
assert(M == (x^2 + y^2 - 3*z^2)*x^2*y^2 + z^6);
-- [Rez00] p. 8
assert(Rob == x^6 + y^6 + z^6 - (x^4*y^2 + x^2*y^4 + x^4*z^2 + x^2*z^4 + y^4*z^2 + y^2*z^4) + 3*x^2*y^2*z^2);
-- [Rez00] p. 11
assert(Sch == 200*(x^3 - 4*x*z^2)^2 + 200*(y^3 - 4*y*z^2)^2 + (y^2 - x^2)*x*(x + 2*z)*(x^2 - 2*x*z + 2*y^2 - 8*z^2));
-- [Sch16] Thm. 2.1 (x0,x1,x2 = x,y,z)
assert(Sc == x^4 + x*y^3 + y^4 - 3*x^2*y*z - 4*x*y^2*z + 2*x^2*z^2 + x*z^3 + y*z^3 + z^4);
-- degrees and homogeneity
for g in {M, Rob, Sch} do assert(isHomogeneous g and first degree g == 6);
assert(isHomogeneous Sc and first degree Sc == 4);
assert(isHomogeneous H and first degree H == 10);
-- Harris: an even symmetric ternary decic ...
assert all(exponents H, e -> all(e, i -> even i));
assert all(permutations 3, pm -> sub(H, {x => (gens R)#(pm#0), y => (gens R)#(pm#1), z => (gens R)#(pm#2)}) == H);
-- ... whose restriction to points (x,x,y) is R_{2,0}(x,x,y)/8 of [Har99]
-- p. 224: R_{u,v}(x,x,y) = 2(v x^2+y^2)(u x^2-y^2)^2((uv+4u-7v-10)x^2+(5uv+2u+v+4)y^2)^2
assert(sub(H, {z => y, y => x}) == 1/8 * 2*y^2*(2*x^2 - y^2)^2*((8 - 10)*x^2 + (4 + 4)*y^2)^2);
assert(sub(H, {z => y, y => x}) == y^2*(2*x^2 - y^2)^2*(x^2 - 4*y^2)^2);
-- the restriction to (x,x,y) is injective on even symmetric ternary decics,
-- so the previous identity determines H uniquely as R_{2,0}/8
orbSum = e -> sum(unique permutations e, p -> x^(p#0)*y^(p#1)*z^(p#2));
basis5 = apply({{10,0,0},{8,2,0},{6,4,0},{6,2,2},{4,4,2}}, orbSum);
restr = apply(basis5, b -> sub(b, {z => y, y => x}));
assert(rank last coefficients matrix {restr} == 5);

S4 = QQ[x,y,z,w];
LL = library("Lax-Lax", S4);
CL = library("Choi-Lam", S4);
-- [Rez00] p. 11: A(x1..x5) = sum_i prod_{j != i} (x_i - x_j) is a form in the
-- differences; the quaternary quartic is A(x,y,z,w,0)
v5 = {x, y, z, w, 0_S4};
Afive = sum(5, i -> product(select(5, j -> j != i), j -> v5#i - v5#j));
assert(LL == Afive);
assert(isHomogeneous LL and first degree LL == 4);
-- [Rez00] p. 10: Q(x,y,z,w) = x^2y^2 + x^2z^2 + y^2z^2 + w^4 - 4xyzw
assert(CL == x^2*y^2 + x^2*z^2 + y^2*z^2 + w^4 - 4*x*y*z*w);
assert(isHomogeneous CL and first degree CL == 4);

-------------------------------------------------------------------------
-- both methods: library(name, Ring) and library(name, List)
use R;
for nm in {"Motzkin", "Robinson", "Schmuedgen", "Scheiderer", "Harris"} do (
    assert(library(nm, R) == library(nm, {x,y,z}));
    -- other variables: a substitution
    assert(library(nm, {y,z,x}) == sub(library(nm, R), {x => y, y => z, z => x}));
    -- dehomogenization as in [JSAG] sec. 4
    assert(library(nm, {x,1_R,z}) == sub(library(nm, R), {y => 1}));
    );
use S4;
for nm in {"Lax-Lax", "Choi-Lam"} do (
    assert(library(nm, S4) == library(nm, {x,y,z,w}));
    assert(library(nm, {w,x,y,z}) == sub(library(nm, S4), {x => w, y => x, z => y, w => z}));
    );
-- [JSAG] sec. 4 dehomogenized Motzkin
U = QQ[symbol x, symbol z];
assert(library("Motzkin", {x,1_U,z}) == x^4 + x^2 + z^6 - 3*x^2*z^2);
-- over the reals (as in the sosdecTernary example)
RR3 = RR[symbol x, symbol y, symbol z];
MR = library("Motzkin", {x,y,z});
assert(ring MR === RR3);
assert(MR == x^4*y^2 + x^2*y^4 + z^6 - 3*x^2*y^2*z^2);
assert(library("Robinson", RR3) == sub(Rob, RR3));

-------------------------------------------------------------------------
-- nonnegativity
use R;
-- [Rez00] p. 8: (x^2+y^2)^2 M = x^2y^2(x^2+y^2+z^2)(x^2+y^2-2z^2)^2 + (x^2-y^2)^2 z^6
assert((x^2+y^2)^2*M == x^2*y^2*(x^2+y^2+z^2)*(x^2+y^2-2*z^2)^2 + (x^2-y^2)^2*z^6);
-- [Sch16] Ex. 3.9: f h = g1^2 + g2^2 + g3^2 with h positive definite
g1 = 4*x^3 + y^3 + z^3 + 4*x*z^2 - 4*y^2*z - 6*x*y*z;
g2 = -4*x^3 + 3*y^3 + 4*z^3 - 3*x^2*y - x*y^2 + x^2*z - x*z^2 + 4*y^2*z + 3*y*z^2 - 2*x*y*z;
g3 = -4*y^3 + 3*z^3 - 3*x^2*y - 7*x*y^2 + 7*x^2*z + 3*x*z^2 + 3*y*z^2 + 8*x*y*z;
hS = 32*x^2 + 24*x*y - 8*x*z + 26*y^2 + 16*y*z + 26*z^2;
assert(Sc * hS == g1^2 + g2^2 + g3^2);
assert(86*hS == 43*(8*x + 3*y - z)^2 + (43*y + 19*z)^2 + 1832*z^2);
-- sampled values on an integer grid
grid3 = flatten flatten apply(toList(-3..3), i -> apply(toList(-3..3), j -> apply(toList(-3..3), k -> {i,j,k})));
for g in {M, Rob, Sch, Sc, H} do
    assert all(grid3, p -> sub(g, {x => p#0, y => p#1, z => p#2}) >= 0);
use S4;
grid4 = flatten flatten flatten apply(toList(-2..2), i -> apply(toList(-2..2), j -> apply(toList(-2..2), k -> apply(toList(-2..2), l -> {i,j,k,l}))));
for g in {LL, CL} do
    assert all(grid4, p -> sub(g, {x => p#0, y => p#1, z => p#2, w => p#3}) >= 0);

-- the multiplied forms of [Rez00] p. 8-9 are sums of squares: solveSOS
-- finds a certificate (exact, or numerical if the rounding fails, since these
-- forms have zeros)
anyCert = (f, sol) -> (
    Q := sol#GramMatrix;
    Q =!= null and (if ring Q === QQ then (isPSDQQ Q and
            transpose(sol#Monomials)*Q*sol#Monomials - f == 0 and value sosPoly sol == f)
        else (sp := sosPoly sol;
            min toList eigenvalues(Q, Hermitian => true) > -1e-6 and
            clean(1e-6, value sp - sub(f, ring sp)) == 0)));
use R;
assert anyCert((x^2+y^2)^2*M, solveSOS((x^2+y^2)^2*M));
RobAff = sub(Rob, {z => 1});
assert anyCert((x^2+y^2)*RobAff, solveSOS((x^2+y^2)*RobAff));

-------------------------------------------------------------------------
-- zeros
use R;
-- Motzkin: (1,+-1,+-1), (1,0,0), (0,1,0) are all the zeros
zM = {{1,1,1},{1,1,-1},{1,-1,1},{1,-1,-1},{1,0,0},{0,1,0}};
assert all(zM, p -> singZero(M, apply(p, c -> c_QQ)));
assert projDistinct zM;
assert(singDegree M == #zM);
-- Robinson: the ten zeros Z of [Rez00] p. 9
zR = {{1,1,1},{1,1,-1},{1,-1,1},{1,-1,-1},{1,1,0},{1,-1,0},{1,0,1},{1,0,-1},{0,1,1},{0,1,-1}};
assert all(zR, p -> singZero(Rob, apply(p, c -> c_QQ)));
assert projDistinct zR;
assert(singDegree Rob == 10);
-- Schmuedgen: eight zeros ([Rez00] p. 11), at 8 of the 9 common zeros of
-- x^3-4xz^2 and y^3-4yz^2
zS = {{0,0,1},{0,2,1},{0,-2,1},{-2,0,1},{-2,2,1},{-2,-2,1},{2,2,1},{2,-2,1}};
assert all(zS, p -> singZero(Sch, apply(p, c -> c_QQ)));
assert(sub(Sch, {x => 2, y => 0, z => 1}) != 0);
assert projDistinct zS;
assert(singDegree Sch == 8);
-- Harris: 30 projective zeros (doc): the orbits of (1,1,0), (1,1,1/2) and
-- (1,1,sqrt 2) ([Har99] Lemma 6.8 with u = 2), and no other singular points
A = QQ[symbol r]/ideal(r^2 - 2);
zH = orbit {{1_A,1_A,0_A}, {2_A,2_A,1_A}, {1_A,1_A,r}};
assert(#zH == 30);
assert projDistinct zH;
assert all(zH, p -> singZero(H, p));
assert(singDegree H == 30);
-- (1,1,z): H(1,1,z) = z^2 (4z^2-1)^2 (z^2-2)^2
T = QQ[symbol s];
assert(sub(H, {x => 1, y => 1, z => s}) == s^2*(4*s^2 - 1)^2*(s^2 - 2)^2);

-------------------------------------------------------------------------
-- "not a sum of squares" (doc): solveSOS must not claim a decomposition
assert noSOSclaim M;
assert noSOSclaim Rob;
assert noSOSclaim Sch;
assert noSOSclaim LL;
assert noSOSclaim CL;
-- [Har99] Lemma 6.8: R_{u,0}, 0 < u < 5/2, u != 1, is not sos (here u = 2)
assert noSOSclaim H;
-- the dehomogenized Motzkin polynomial is not SOS either
assert noSOSclaim library("Motzkin", {x, 1_R, z});
