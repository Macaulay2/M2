-- Return shapes of the documented calls (bench/interface-original.txt,
-- section 7): classes, sequence lengths, which entries of an SDPResult are
-- null, the ring of the monomials and the coefficient field of the Gram
-- matrix.  Left out on purpose: sizes of Gram matrices chosen by the Newton
-- polytope, numbers of squares, and the entries whose change is decided
-- (BUGS.md 10: Parameters without parameters; 18: the multiplier parameters
-- of sosInIdeal; 21: recoverSolution of a failed result); those are in
-- tests/open/.

isQQ = M -> instance(M, Matrix) and ring M === QQ
isRR = M -> instance(M, Matrix) and instance(ring M, RealField)
colOver = (M, S) -> instance(M, Matrix) and ring M === S and numColumns M == 1

-- solveSOS of an SOS polynomial over QQ: exact Gram matrix, real moment matrix
R = QQ[x,y];
f = 2*x^4+5*y^4-2*x^2*y^2+2*x^3*y;
sol = solveSOS f;
assert(class sol === SDPResult)
assert colOver(sol#Monomials, R)
assert(isQQ sol#GramMatrix and numRows sol#GramMatrix == numRows sol#Monomials)
assert(numColumns sol#GramMatrix == numRows sol#GramMatrix)
assert(isRR sol#MomentMatrix and numRows sol#MomentMatrix == numRows sol#Monomials)
assert(sol#Parameters === null)
assert instance(sol#Status, String)
assert(status sol === sol#Status)
s = sosPoly sol;
assert(class s === SOSPoly and ring s === R)
assert(all(gens s, g -> ring g === R) and all(coefficients s, c -> ring c === QQ))
assert(length s == #gens s and #gens s == #coefficients s)
assert(ring value sosPoly(R, {x+1,y}, {2,3}) === R)
assert(class sosPoly(matrix{{1},{x},{y}}, matrix(QQ,{{1,1,1},{1,1,1},{1,1,1}})) === SOSPoly)
assert(class sosPoly({x+1,y}, {2,3}) === SOSPoly)
assert instance(net sol, Net)
assert instance(net s, Net)
assert instance(expression s, Expression)

-- not SOS: the monomials are set, the Gram matrix is null, sosPoly gives null
sol = solveSOS(x^2*y^2-1);
assert(class sol === SDPResult)
assert colOver(sol#Monomials, R)
assert(sol#GramMatrix === null and sol#Parameters === null)
assert instance(sol#Status, String)
assert(sosPoly sol === null)
-- an odd vertex of the Newton polytope: no Gram matrix (in the original all
-- five entries are null; that undocumented detail is not pinned)
sol = solveSOS(x^3);
assert(class sol === SDPResult and sol#GramMatrix === null and sosPoly sol === null)

-- parameters: the parameter values are a column over QQ, one entry per parameter
R = QQ[x][t];
sol = solveSOS((t-1)*x^4+1/2*t*x+1);
assert(isQQ sol#GramMatrix and isRR sol#MomentMatrix)
assert(isQQ sol#Parameters and numRows sol#Parameters == 1 and numColumns sol#Parameters == 1)
assert(ring sol#Monomials === coefficientRing R) -- the monomials are in x only
sol = solveSOS(x^2-3*x-t, -t, RoundTol=>12);
assert(isQQ sol#GramMatrix and isQQ sol#Parameters and numRows sol#Parameters == 1)
R = QQ[x,y,z][symbol s, symbol t];
sol = solveSOS(library("Robinson", {x,y,z}) + s*x^6 + t*y^6);
assert(isQQ sol#Parameters and numRows sol#Parameters == 2 and numColumns sol#Parameters == 1)

-- quotient rings: the monomials stay in the quotient ring
R = QQ[x,y]/ideal(x^2 + y^2 - 1);
f = 10-x^2-y;
mon = matrix{{1},{x},{y}};
for sol in {solveSOS(f, mon), solveSOS(f, 2), solveSOS(f, 2, TraceObj => true)} do (
    assert colOver(sol#Monomials, R);
    assert(isQQ sol#GramMatrix and isRR sol#MomentMatrix and sol#Parameters === null));
-- with a degree bound D: all monomials of degree at most D/2 (documented)
assert(set flatten entries (solveSOS(f, 2))#Monomials === set {1_R, x, y})
-- parameters in a quotient ring
S = R[t];
sol = solveSOS(f-t, -t, mon, RoundTol=>12); -- mon in R, as documented (a mon in S fails, BUGS.md 22)
assert(isQQ sol#Parameters and numRows sol#Parameters == 1)
-- RoundTol => infinity: real result, the monomials in a new ring over RR with
-- the same variable names, not a quotient ring (documented caveat)
sol = solveSOS(f, mon, RoundTol=>infinity);
assert(isRR sol#GramMatrix and isRR sol#MomentMatrix)
R' = ring sol#Monomials;
assert(R' =!= R and instance(coefficientRing R', RealField))
assert(toString gens R' == toString {x, y} and not isQuotientRing R')
assert(ring sosPoly sol === R')

-- sosInIdeal: (SDPResult, multipliers as a column, one per generator)
R = QQ[x,y,z];
h = matrix {{x^2-4*x+2*y^2, 2*z^2-y^2+2}};
setRandomSeed 0;
r = sosInIdeal(h, 2);
assert(class r === Sequence and #r == 2)
(sol, mult) = r;
assert(class sol === SDPResult and isQQ sol#GramMatrix and colOver(sol#Monomials, R))
assert(colOver(mult, R) and numRows mult == numColumns h)
S = R/ideal h;
setRandomSeed 0;
sol = sosInIdeal(S, 2);
assert(class sol === SDPResult and isQQ sol#GramMatrix and colOver(sol#Monomials, S))
assert(sol#Parameters === null)
-- no SOS in the ideal: (SDPResult with null Gram matrix, null)
R = QQ[x,y];
setRandomSeed 0;
r = sosInIdeal(matrix{{x^2-y}}, 2);
assert(class r === Sequence and #r == 2 and class r#0 === SDPResult)
assert(r#0#GramMatrix === null and r#1 === null)

-- sosdecTernary: two lists of SOSPoly; (null, null) if f is not nonnegative
R = RR[x,y,z];
setRandomSeed 0;
r = sosdecTernary library("Motzkin", {x,y,z});
assert(class r === Sequence and #r == 2)
assert(all(r, l -> class l === List and #l > 0 and all(l, p -> class p === SOSPoly)))
R = QQ[x,y,z];
setRandomSeed 0;
assert(sosdecTernary(x^4*y^2+x^2*y^4+z^6-4*x^2*y^2*z^2) === (null, null))

-- lowerBound: (bound, SDPResult) or (bound, SDPResult, mult); the bound in QQ
R = QQ[x];
r = lowerBound((x-1)^2 + (x+3)^2);
assert(class r === Sequence and #r == 2)
assert(ring r#0 === QQ and class r#1 === SDPResult)
assert(isQQ r#1#GramMatrix and isQQ r#1#Parameters)
p = recoverSolution r#1;
assert(class p === List and #p == 1 and class p#0 === Option and p#0#0 === x)
assert instance(p#0#1, RR)
r = lowerBound((x^2-x)/(x^2+1));
assert(#r == 2 and ring r#0 === QQ and class r#1 === SDPResult)
r = lowerBound((x-1)^2 + (x+3)^2, RoundTol => infinity);
assert(#r == 2 and instance(r#0, RR) and isRR r#1#GramMatrix)
R = QQ[x,y]/ideal(x^2 - x, y^2 - y);
r = lowerBound(x - y, 2);
assert(#r == 2 and ring r#0 === QQ and colOver(r#1#Monomials, R))
R = QQ[x,y];
r = lowerBound(x - y, matrix{{x^2 - x, y^2 - y}}, 2);
assert(class r === Sequence and #r == 3)
assert(ring r#0 === QQ and class r#1 === SDPResult and colOver(r#2, R) and numRows r#2 == 2)
-- failures: null bound
r = lowerBound(x^3);
assert(#r == 2 and r#0 === null and class r#1 === SDPResult and r#1#GramMatrix === null)
r = lowerBound(x^3, matrix{{x^2-1}}, 2);
assert(#r == 3 and r#0 === null and r#1#GramMatrix === null and r#2 === null)

-- recoverSolution(mon, X): a list of options variable => real number
R = RR[x,y];
p = recoverSolution(matrix{{1},{x},{y}}, matrix(RR, {{1,0,1},{0,0,0},{1,0,1}}));
assert(class p === List and #p == 2 and all(p, o -> class o === Option))
assert(set apply(p, o -> o#0) === set {x, y})

-- clean: an SOSPoly in the same ring
s = sosPoly(R, {x^2+.0001*x+1, y}, {2, .0001});
assert(class clean(0.001, s) === SOSPoly and ring clean(0.001, s) === R)

-- library: a form of the given ring, both methods
R = QQ[x,y,z,w];
for n in {"Motzkin", "Robinson", "Schmuedgen", "Lax-Lax", "Choi-Lam", "Scheiderer", "Harris"} do (
    g := library(n, R);
    assert(ring g === R and isHomogeneous g and g == library(n, gens R)))

-- checkSolver(solver, fun): a list of booleans
c = checkSolver("CSDP", "solveSOS");
assert(class c === List and #c > 0 and all(c, b -> instance(b, Boolean)))
