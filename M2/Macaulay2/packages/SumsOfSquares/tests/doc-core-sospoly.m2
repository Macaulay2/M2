-- doc-core-sospoly.m2 -- independent tests written from the documentation
-- (node SOSPoly, sosPoly, value(SOSPoly), clean(RR,SOSPoly), [JSAG] sec. 1).
-- Checks mathematical properties of the type SOSPoly: construction, accessors,
-- expansion, arithmetic, equality, substitution, clean.
-- Self-contained; assumes SumsOfSquares is loaded.
setRandomSeed 0;  -- fixed seed for anything random in the solver path

-- exact PSD test over QQ by successive Schur complements
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
allPos = L -> all(L, c -> c > 0);
-- the defining expansion f = sum_i d_i g_i^2, computed independently
expandSOS = s -> sum(#(gens s), i -> (coefficients s)#i * ((gens s)#i)^2);

-------------------------------------------------------------------------
-- construction from generators and weights (doc example)
R = QQ[x,y];
s = sosPoly(R, {x+1,y}, {2,3});
assert instance(s, SOSPoly);
assert(ring s === R);
assert(gens s == {x+1, y});
assert(coefficients s == {2, 3});
assert(length s == 2);
-- value: f = sum_i d_i g_i^2 (doc of value(SOSPoly))
assert(value s == 2*(x+1)^2 + 3*y^2);
assert(value s == 2*x^2 + 3*y^2 + 4*x + 2);
assert(ring value s === R);
assert(value s == expandSOS s);
-- the type is a hash table with the generators, coefficients and ring
assert(instance(s, HashTable));
assert(#(gens s) == #(coefficients s));

-- sosPoly(List,List): the ring is taken from the generators
s2 = sosPoly({x+1, y}, {2, 3});
assert instance(s2, SOSPoly);
assert(ring s2 === R);
assert(value s2 == value s);
assert(s2 == s);

-- sosPoly(Matrix,Matrix): Gram matrix and monomial vector (doc example)
Q = matrix(QQ, {{1,1,1},{1,1,1},{1,1,1}});
mon = matrix{{1},{x},{y}};
s3 = sosPoly(mon, Q);
assert instance(s3, SOSPoly);
assert(value s3 == (transpose(mon)*Q*mon)_(0,0));
assert(value s3 == (1+x+y)^2);
assert allPos coefficients s3;
-- Q has rank 1 and the products of 1,x,y are distinct monomials, so the
-- quadratic form, hence at least rank(Q) squares, is determined: LDL gives 1
assert(length s3 == 1);

-- round trips sosPoly(mon,Q) -> value for several PSD Gram matrices
roundTrip = (mon, Q) -> (
    t := sosPoly(mon, Q);
    instance(t, SOSPoly) and value t == (transpose(mon)*Q*mon)_(0,0)
      and allPos coefficients t and length t <= numrows Q);
assert roundTrip(matrix{{x},{y}}, matrix(QQ, {{1,1},{1,1}}));
assert roundTrip(matrix{{x},{y}}, matrix(QQ, {{2,1},{1,2}}));
assert roundTrip(matrix{{1},{x},{y}}, matrix(QQ, {{2,1,0},{1,2,1},{0,1,2}}));
assert roundTrip(matrix{{1},{x},{y}}, matrix(QQ, {{0,0,0},{0,1,0},{0,0,0}}));
assert roundTrip(matrix{{1},{x},{x^2}}, matrix(QQ, {{1,0,-1},{0,0,0},{-1,0,1}}));
assert roundTrip(matrix{{1},{x},{y},{x*y}}, matrix(QQ, {{5,1,2,0},{1,4,1/3,1},{2,1/3,6,1/2},{0,1,1/2,7}}));
-- rational Gram matrices with large denominators
Qbig = matrix(QQ, {{1, 1/1000003, 0},{1/1000003, 2, 7/999983},{0, 7/999983, 1/999999}});
assert isPSDQQ Qbig;
assert roundTrip(matrix{{1},{x},{y}}, Qbig);
-- number of squares equals the rank where the quadratic form is determined
s4 = sosPoly(matrix{{x},{y}}, matrix(QQ, {{2,1},{1,2}}));
assert(length s4 == 2);

-- a non-PSD "Gram matrix": x^2+4xy+y^2 is indefinite, so no SOSPoly with
-- positive weights can have this value
r = try sosPoly(matrix{{x},{y}}, matrix(QQ, {{1,2},{2,1}})) else null;
assert(not (instance(r, SOSPoly) and allPos coefficients r and value r == x^2+4*x*y+y^2));

-------------------------------------------------------------------------
-- arithmetic: values behave like the polynomials
t = sosPoly(R, {x-y, x*y+1, 1}, {1/2, 5, 7});
assert(value t == 1/2*(x-y)^2 + 5*(x*y+1)^2 + 7);
-- sum
u = s + t;
assert instance(u, SOSPoly);
assert(value u == value s + value t);
assert allPos coefficients u;
assert(value u == expandSOS u);
-- product
p = s * t;
assert instance(p, SOSPoly);
assert(value p == value s * value t);
assert allPos coefficients p;
assert(value p == expandSOS p);
pp = s * s;
assert(value pp == (value s)^2);
-- scalar multiples (positive rationals and integers)
for c in {2, 1/3, 1000000, 1/1000000, 7/5} do (
    sc := c * s;
    assert instance(sc, SOSPoly);
    assert(value sc == c * value s);
    assert allPos coefficients sc;
    );
-- zero multiple: the zero polynomial
z0 = 0 * s;
assert(value z0 == 0);
-- a negative multiple of a nonzero SOS is not SOS: must not give an SOSPoly
neg = try (-1) * s else null;
assert(not instance(neg, SOSPoly));
neg2 = try (-1/2) * t else null;
assert(not instance(neg2, SOSPoly));
-- even powers
for k in {2, 4, 6} do (
    sk := s^k;
    assert instance(sk, SOSPoly);
    assert(value sk == (value s)^k);
    assert allPos coefficients sk;
    );
-- associativity / distributivity on the level of values
assert(value((s + t) * t) == value(s*t + t*t));
assert(value(2*(s + t)) == value(2*s + 2*t));

-------------------------------------------------------------------------
-- equality: SOSPoly == SOSPoly, == RingElement, == Matrix (1x1)
assert(s == s);
assert(s == value s);
assert(value s == s);
assert(s == matrix{{value s}});
assert(matrix{{value s}} == s);
assert(not (s == value s + 1));
assert(not (value s + 1 == s));
assert(not (s == t));
assert(not (s == matrix{{value t}}));
-- the same polynomial written with other squares
sAlt = sosPoly(R, {2*x+2, y}, {1/2, 3});
assert(value sAlt == value s);
assert(s + t == t + s);
assert(s * t == t * s);

-------------------------------------------------------------------------
-- expression and net
e = expression s;
assert instance(e, Expression);
assert(value e == value s);
assert instance(net s, Net);
assert(width net s > 0);

-------------------------------------------------------------------------
-- substitute into a ring with more variables (doc example)
S = QQ[x,y,z];
ss = sub(s, S);
assert instance(ss, SOSPoly);
assert(ring ss === S);
assert(value ss == sub(value s, S));
assert(value ss == 2*(x+1)^2 + 3*y^2);
assert(coefficients ss == coefficients s);
-- substitute back
sb = sub(ss, R);
assert(ring sb === R);
assert(value sb == value s);

-------------------------------------------------------------------------
-- SOSPoly over a quotient ring
Rq = QQ[symbol a, symbol b]/ideal(a^2 + b^2 - 1);
q = sosPoly(Rq, {a, b}, {1, 1});
assert(ring q === Rq);
assert(value q == 1);
assert(q == 1_Rq);
q2 = sosPoly(Rq, {a + b, 1}, {1, 2});
assert(value q2 == 3 + 2*a*b);

-------------------------------------------------------------------------
-- SOSPoly over RR
T = RR[symbol v, symbol w];
rr = sosPoly(T, {v+1, w}, {2.0, 3.0});
assert(ring rr === T);
assert(length rr == 2);
assert(clean(1e-12, value rr - (2*(v+1)^2 + 3*w^2)) == 0);
assert(clean(1e-12, value(rr + rr) - 2*value rr) == 0);
assert(clean(1e-12, value(rr * rr) - (value rr)^2) == 0);
assert(clean(1e-12, value(0.5 * rr) - 0.5 * value rr) == 0);
assert(rr == rr);

-------------------------------------------------------------------------
-- clean(RR,SOSPoly): doc example
cs = sosPoly(T, {v^2 + .0001*v + 1, w}, {2, .0001});
cc = clean(.001, cs);
assert instance(cc, SOSPoly);
-- the square with weight .0001 < .001 is removed
assert(length cc == 1);
assert(all(coefficients cc, c -> c >= .001));
-- what remains approximates 2*(v^2+1)^2
assert(clean(.001, value cc - 2*(v^2+1)^2) == 0);
-- a tolerance below all weights removes no square
cc2 = clean(1e-5, cs);
assert(length cc2 == 2);
-- weights above the tolerance stay
cc3 = clean(.5, sosPoly(T, {v, w, v+w}, {1.0, 0.25, 3.0}));
assert(length cc3 == 2);
assert(sort coefficients cc3 == {1.0, 3.0});
-- clean does nothing on rational coefficients (doc)
use R;
qs = sosPoly(R, {x + 1/1000000*y, y}, {2, 1/1000000});
cq = clean(.001, qs);
assert(length cq == 2);
assert(value cq == value qs);
assert(coefficients cq == coefficients qs);
assert(gens cq == gens qs);

-------------------------------------------------------------------------
-- sosPoly of an SDPResult ([JSAG] sec. 1: f = sum lambda_i f_i^2)
use R;
f = 2*x^4+5*y^4-2*x^2*y^2+2*x^3*y;
sol = solveSOS f;
sp = sosPoly sol;
assert instance(sp, SOSPoly);
assert(ring sp === R);
assert(value sp == f);
assert allPos coefficients sp;
assert(sum(#(gens sp), i -> (coefficients sp)#i * ((gens sp)#i)^2) == f);
-- sosPoly(SDPResult) agrees with sosPoly(Monomials, GramMatrix)
sp2 = sosPoly(sol#Monomials, sol#GramMatrix);
assert(value sp2 == f);
