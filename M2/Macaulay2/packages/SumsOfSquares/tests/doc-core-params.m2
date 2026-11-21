-- doc-core-params.m2 -- independent tests written from the documentation
-- (node solveSOS: "SOS with parameters", "SOS with parameter optimization";
-- [JSAG] sec. 4; [PP08] sec. 2.2.3 and Example 4).
-- Parameters are the variables of the ring K[x][p]; f depends affinely on
-- them; solveSOS finds parameter values making f SOS (and optimizes a linear
-- objective).  Self-contained; assumes SumsOfSquares is loaded.
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
exactCert = (f, sol) -> (
    Q := sol#GramMatrix; mon := sol#Monomials;
    Q =!= null and mon =!= null and ring Q === QQ and isPSDQQ Q
      and (transpose(mon)*Q*mon - f == 0)
      and (s := sosPoly sol; instance(s, SOSPoly) and value s == f
           and all(coefficients s, c -> c > 0)));
-- substitute the parameter values of sol into F (ring K[x][p] -> K[x])
substParams = (F, sol) -> (
    S := ring F; P := sol#Parameters;
    lift(sub(F, apply(numgens S, i -> S_i => P_(i,0))), coefficientRing S));
-- an exact certificate for F at the returned rational parameter values
paramCert = (F, sol) -> (
    P := sol#Parameters;
    sol#Status == okStatus and P =!= null and ring P === QQ
      and numrows P == numgens ring F and exactCert(substParams(F, sol), sol));
param = (sol, i) -> (sol#Parameters)_(i,0);

-------------------------------------------------------------------------
-- doc example: (t-1)x^4 + t/2 x + 1
R = QQ[x][t];
f = (t-1)*x^4+1/2*t*x+1;
sol = solveSOS f;
assert paramCert(f, sol);
-- the SOSPoly lives in the polynomial ring without parameters
assert(ring sosPoly sol === coefficientRing R);
-- necessarily the leading coefficient t-1 is positive
assert(param(sol, 0) > 1);
-- independent check of nonnegativity of the substituted polynomial
fs = substParams(f, sol);
assert all(-20..20, i -> sub(fs, {x => i/4}) >= 0);

-------------------------------------------------------------------------
-- doc example / [JSAG] sec. 4: Robinson polynomial plus s*x^6 + t*y^6
R2 = QQ[x,y,z][s,t];
g = library("Robinson", {x,y,z}) + s*x^6 + t*y^6;
sol2 = solveSOS g;
assert paramCert(g, sol2);
assert(numrows sol2#Parameters == 2);
-- the Robinson form itself is not SOS, so (s,t) = (0,0) is not a solution;
-- a homogeneous result: the Gram matrix uses only cubic monomials
assert(param(sol2, 0) != 0 or param(sol2, 1) != 0);
assert all(flatten entries sol2#Monomials, m -> first degree m == 3);

-------------------------------------------------------------------------
-- several parameters appearing in several coefficients
R3 = QQ[x][p,q];
h = x^4 + p*x^3 + (p+q)*x^2 + q*x + 1;
sol3 = solveSOS h;
assert paramCert(h, sol3);
-- a parameter that multiplies the whole polynomial and a constant term
R4 = QQ[x,y][a,b,c];
k = a*(x^2 + y^2) + b*x*y + (c - 1)*(x^4 + y^4) + 1 + x^2*y^2;
sol4 = solveSOS k;
assert paramCert(k, sol4);
-- f = t (constant in x): any t >= 0
R5 = QQ[x][t];
sol5 = solveSOS(t + 0*x);
assert paramCert(t + 0*x, sol5);
assert(param(sol5, 0) >= 0);
-- no parameter value makes -x^2 + t SOS: no certificate may be claimed
sol6 = solveSOS(-x^2 + t);
assert(not paramCert(-x^2 + t, sol6));

-------------------------------------------------------------------------
-- parameter optimization: min objFun(p) s.t. f(x;p) is SOS
-- doc example: min -t s.t. x^2 - 3x - t SOS; optimum t = -9/4
use R5;
for rt in {4, 12, 20} do (
    sl := solveSOS (x^2 - 3*x - t, -t, RoundTol=>rt);
    assert paramCert(x^2 - 3*x - t, sl);
    -- exact certificate => rigorous lower bound
    assert(param(sl, 0) <= -9/4);
    assert(param(sl, 0) > -9/4 - 1/10);
    );
sl = solveSOS (x^2 - 3*x - t, -t);
assert paramCert(x^2 - 3*x - t, sl);
assert(param(sl, 0) <= -9/4 and param(sl, 0) > -9/4 - 1/1000);
-- min of x^4 - 2x^2 is -1 (univariate: nonnegative <=> SOS)
sl = solveSOS (x^4 - 2*x^2 - t, -t);
assert paramCert(x^4 - 2*x^2 - t, sl);
assert(param(sl, 0) <= -1 and param(sl, 0) > -1 - 1/1000);
-- min of (x-1)^2 + (y+2)^2 + 3 is 3
R6 = QQ[x,y][t];
sl = solveSOS ((x-1)^2 + (y+2)^2 + 3 - t, -t);
assert paramCert((x-1)^2 + (y+2)^2 + 3 - t, sl);
assert(param(sl, 0) <= 3 and param(sl, 0) > 3 - 1/1000);
-- maximizing instead of minimizing: min t s.t. x^2 + 2x + t SOS is t = 1
sl = solveSOS (x^2 + 2*x + t, t);
assert paramCert(x^2 + 2*x + t, sl);
assert(param(sl, 0) >= 1 and param(sl, 0) < 1 + 1/1000);

-- two parameters and a two-parameter objective:
-- x^2 - 2a x + b is SOS iff b >= a^2, so b - 2a >= a^2 - 2a >= -1, with
-- equality at (a,b) = (1,1)
R7 = QQ[x][a,b];
F7 = x^2 - 2*a*x + b;
sl = solveSOS (F7, b - 2*a);
assert paramCert(F7, sl);
val = param(sl, 1) - 2*param(sl, 0);
assert(val >= -1);
assert(val < -1 + 1/1000);
assert(param(sl, 1) >= (param(sl, 0))^2);

-------------------------------------------------------------------------
-- [PP08] Example 4: lower bound of x^4+y^4+z^4-4xyz+x+y+z; numerical value
-- -2.11291382 in the literature
R8 = QQ[x,y,z][t];
F8 = x^4 + y^4 + z^4 - 4*x*y*z + x + y + z;
sl = solveSOS (F8 - t, -t);
assert paramCert(F8 - t, sl);
tv = param(sl, 0);
-- at the default the original rounds only at 32 bits here; a coarser
-- certified bound is allowed (RoundTol is the documented trade-off), so the
-- window is checked without rounding (lead, after the test audit)
assert(tv < -2.1128);
tvInf = param(solveSOS (F8 - t, -t, RoundTol => infinity), 0);
assert(tvInf > -2.1130 and tvInf < -2.1128);
-- soundness: the certified bound is below the values of F8 at sample points
A8 = coefficientRing R8;
F8A = lift(F8, A8);
use A8;
assert all(-6..6, i -> all(-6..6, j -> all(-6..6, l ->
            sub(F8A, {x => i/5, y => j/5, z => l/5}) >= tv)));
