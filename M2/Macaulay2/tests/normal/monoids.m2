--- checking variable processing
S = monoid[a_(0,0)..c_(2,2),x_0..z_3]
assert(numgens S == 39)
assert match(regexQuote "a_(0,0)..c_(2,2), x_0..z_3", toExternalString S)
---
monoid[a,b,c]
monoid[a..c,x..z]
monoid[(a,b,c),(x,y,z)]
monoid[{a,b,c},{x,y,z}]
monoid[vars(0..2,23..25)]
monoid[vars(0..2),vars(23..25)]
monoid[a_0..a_3,b_0..b_3] -- FIXME: this would fail if QQ[a,b,c] was defined before
---
monoid[a,b, WeylAlgebra => a=>b]
monoid[a,b, WeylAlgebra => 0=>1]
monoid[Variables => 4, WeylAlgebra => {{0, 2}, {1, 3}}]
monoid[Variables => 4, WeylAlgebra => {(0, 2), (1, 3)}]
monoid[Variables => 4, WeylAlgebra => {0 => 2, 1 => 3}]
---
monoid[Variables => 4, VariableBaseName => "e", SkewCommutative => false]
monoid[Variables => 4, VariableBaseName => "e", SkewCommutative => true]
monoid[Variables => 4, VariableBaseName => "e", SkewCommutative => 0..3]
monoid[Variables => 4, VariableBaseName => "e", SkewCommutative => {0,1,2,3}]
monoid[vars(0..3), VariableBaseName => "e", SkewCommutative => {0,1,2,3}]
monoid[vars(0..3), VariableBaseName => "e", SkewCommutative => vars(0..3)]
---

--- checking element access in towers
-* currently not working reliably
R = ((frac(QQ[x,y]))[t,u])/(t^2-x,u^2-y)
assert(sum({"x", "y", "t", "u"}, s -> s_R) === t + u + x + y)

-- promote
F = frac(QQ[x,y])
S = F[t,u]
R = S/(t-x)
assert all({"x", "y"}, s -> promote(s_(monoid F), R) === s_R)
assert all({"t", "u"}, s -> promote(s_(monoid S), R) === s_R) -- monoid S === S.FlatMonoid
assert all({"t", "u"}, s -> promote(s_(monoid R), R) === s_R)
assert all({"t", "u"}, s -> promote(s_(R.FlatMonoid), R) === s_R)

-- lift
F = frac(kk[a,b])
S = F[x,y]
R = S[t,u]/(x^4-t*a*y)
-- TODO
*-

---- checking degreeLength, degreesMonoid, degreesRing
assert(degreeLength degreesMonoid 0 == 0)
assert(degreeLength degreesMonoid 2 == 0)
assert(degreeLength degreesMonoid{} == 0)
assert(degreeLength degreesMonoid{1} == 1)
assert(degreeLength degreesMonoid{1,2} == 1)

assert(degreeLength degreesRing 0 == 0)
assert(degreeLength degreesRing 2 == 0)
assert(degreeLength degreesRing{} == 0)
assert(degreeLength degreesRing{1} == 1)
assert(degreeLength degreesRing{1,2} == 1)

assert(degreeLength(monoid[]) == 1)
assert(degreeLength(monoid[a]) == 1)
assert(degreeLength(monoid[a,b]) == 1)

---- checking degree groups
--- monoid with free degree group
A = monoid[x,y,z, Degrees => {{1,1}, {-1,1}, {0,1}}]
assert(degreeGroup A == ZZ^2)
assert(degree A_{3,0,0} == {3,3})

--- monoid with torsion degree group
G = coker matrix{{3}, {0}}
A = monoid[x,y,z,
    DegreeGroup => G,
    Degrees => {{1,1}, {-1,1}, {0,1}}]
assert(degreeGroup A == G)
assert(degree A_{3,0,0} == {0,3})
assert(heft A == {0,1})

-- test heft vectors in presence of torsion degree groups
assert(heft(monoid[x,y, DegreeGroup => coker matrix"0;1", Degrees => {2:{ 1, 1}}]) == { 1, 0})
assert(heft(monoid[x,y, DegreeGroup => coker matrix"0;1", Degrees => {2:{ 1,-1}}]) == { 1, 0})
assert(heft(monoid[x,y, DegreeGroup => coker matrix"0;1", Degrees => {2:{-1, 1}}]) == {-1, 0})
assert(heft(monoid[x,y, DegreeGroup => coker matrix"1;0", Degrees => {2:{ 1, 1}}]) == { 0, 1})
assert(heft(monoid[x,y, DegreeGroup => coker matrix"1;0", Degrees => {2:{ 1,-1}}]) == { 0,-1})
assert(heft(monoid[x,y, DegreeGroup => coker matrix"1;0", Degrees => {2:{-1, 1}}]) == { 0, 1})

--- tensor of monoids with torsion
B = A ** A
assert(degreeGroup B == G ++ G)
assert(degree B_{3,0,0,3,0,0} == {0,3,0,3})
assert(heft B == {0,1,0,1})

--- ring with torsion degree group
S = (ZZ/3) A
T = (ZZ/3) B

-- module over ring with torsion degree group
assert(degrees S^{{3,3}} == {{0,-3}})
assert(S^{{3,3}} == S^{{0,3}})
-- assert(S^{{3,3}} === S^{{0,3}}) -- TODO: should this be true?

--- testing graphIdeal
f = map(T, S, {T_0, T_1, T_2}, DegreeMap => d -> join(d, {0,0}))
assert isHomogeneous f
I = graphIdeal f
assert isHomogeneous I
assert(degreeGroup monoid T == degreeGroup monoid ring I)

g = map(S, T, {S_0, S_1, S_2, S_0, S_1, S_2}, DegreeMap => d -> sum pack_2 d)
assert isHomogeneous g
J = graphIdeal g
assert isHomogeneous J
assert(degreeGroup monoid S == degreeGroup monoid ring J)

--- TODO
use S
R = S/(x-y)
monoid R

--- test Cox rings with torsion degree group
needsPackage "NormalToricVarieties"
B = {{2, -1}, {-1, 2}, {-1,-1}}
X = normalToricVariety(B, {{0, 1}, {1, 2}, {2, 0}})
G = classGroup X -- prune coker matrix B
A = fromWDivToCl X -- G.cache.pruningMap^-1
S = ring X -- :)
assert(degreeGroup S == G)
assert(degrees S == transpose entries A)

-- test basis
L = monomials(D = 3*X_0)
assert(set first entries basis(degree D, S) === set L)
-- FIXME should be homogeneous
isHomogeneous sum({1,1,1,1}, L, times)

-- test truncate
needsPackage "Truncations"
assert(set(truncate({0,3}, S))_* === set(monomials(X_0+2*X_1) | monomials(2*X_0+X_1) | monomials(3*X_0)))

--- test passing a map of ZZ-modules for Degrees
M = monoid[a,b,c, Degrees => A]
assert(degreeGroup M == G)
assert(degrees M == degrees S)

--- test passing degrees from the degree rank
M = monoid[a,b,c, DegreeGroup => G]
assert(degreeGroup M == G)
assert(degrees M == splice {{1, 0}, 2:{0, 1}})

M = monoid[a,b,c, DegreeRank => 2]
assert(degreeGroup M == ZZ^2)
assert(degrees M == splice {{1, 0}, 2:{0, 1}})

--- test passing degrees from generators of the degree group
H = subquotient(ambient A, relations G)
M = monoid[a,b,c, DegreeGroup => H]
assert(degreeGroup M == G)
assert(degrees M == degrees S)

--- test symmetric algebra
R = (ZZ/101) M
assert(degreeGroup R == G)
-- TODO: make sure this is correct
assert(degreeGroup monoid symmetricAlgebra image vars R == ZZ^3)

--- test the Join option
M = monoid[]
N = monoid[Join=>false]
-- Associativity of tensor is important:
-- The left hand operand is the only one where Join is obeyed, as it corresponds
-- to the most recently adjoined monoid in a tower of polynomial rings.
assert(1 == degreeLength tensor(tensor(N, M), M ))
assert(1 == degreeLength tensor(N, tensor(M,  M)))
assert(3 == degreeLength tensor(tensor(M, N), M ))
assert(3 == degreeLength tensor(M, tensor(M,  N)))
assert(3 == degreeLength tensor(tensor(M, M), N ))
--
M = monoid[b, Degrees => {{1, 2}}]
A = monoid[a, Degrees => {{1, 0, 0}}, Join => false]
degreeLength tensor(A, M) == 3
degreeLength tensor(A, M, Degrees => {{1,0},{0,1}}) == 2
degreeLength tensor(A, M, DegreeRank => 3) == 3
degreeLength tensor(A, M, DegreeGroup => coker matrix {{0}, {2}, {3}}) == 3
B = monoid[a, Degrees => {{1, 0, 0}}]
degreeLength tensor(B, M) == 5
degreeLength tensor(A, M, Degrees => {{1,0},{0,1}}) == 2
degreeLength tensor(A, M, DegreeRank => 3) == 3
degreeLength tensor(A, M, DegreeGroup => coker matrix {{0}, {2}, {3}}) == 3
C = monoid[a, Degrees => {{1, 0, 0}}, Join => true]
degreeLength tensor(C, M) == 5
degreeLength tensor(A, M, Degrees => {{1,0},{0,1}}) == 2
degreeLength tensor(A, M, DegreeRank => 3) == 3
degreeLength tensor(A, M, DegreeGroup => coker matrix {{0}, {2}, {3}}) == 3

-- test newRing
G = coker matrix{{3}, {0}}
R = QQ[x,y,z, Degrees => {{1,1}, {-1,1}, {0,1}}]
S = newRing(R, DegreeGroup => G)
assert(degreeGroup S == G)
assert(degrees S == degrees R)
T = newRing(S, Degrees => degrees S)
assert(degreeGroup T == ZZ^2)
U = newRing(S, DegreeRank => 3)
assert(degreeGroup U == ZZ^3)

-- test tensor
U = QQ[x, dx, WeylAlgebra => x => dx]
A = monoid U
assert(U^1 == Hom(U^1, U^1))
assert(U.WeylAlgebra == {{0, 1}})
assert(A.Options.WeylAlgebra == {{A_0, A_1}})
W = U ** U
B = monoid W
assert(W^1 == Hom(W^1, W^1))
assert(W.WeylAlgebra == {{0, 1}, {2, 3}})
assert(B.Options.WeylAlgebra == {{B_0, B_1}, {B_2, B_3}})

E = QQ[u, v, SkewCommutative => true]
A = monoid E
assert(E^1 == Hom(E^1, E^1))
assert(E.SkewCommutative == {0, 1})
assert(A.Options.SkewCommutative == {0, 1})
F = E ** E
B = monoid F
assert(F^1 == Hom(F^1, F^1))
assert(F.SkewCommutative == {0, 1, 2, 3})
assert(B.Options.SkewCommutative == {0, 1, 2, 3})

-- test variable deduplication
assert(gens(A = monoid[x,y]) == {x,y})
assert(gens(B = A ** A) == {x_0,y_0,x_1,y_1}) -- TODO: eliminate the warning
assert(gens(C = B ** B) == {x_(0,0),y_(0,0),x_(0,1),y_(0,1),x_(1,0),y_(1,0),x_(1,1),y_(1,1)})
assert(toString gens(A ** B) == "{x, y, x_0, y_0, x_1, y_1}") -- TODO: not yet working without toString
assert(toString gens(monoid[x,y,x,z]) == "{x_0, y, x_1, z}") -- TODO: not yet working without toString
assert(toString gens(monoid[x,x,x]) == "{x_0, x_1, x_2}") -- issue #3261

-- test ^** and runLengthEncode
-- TODO: what should happen for odd powers?
A = (monoid[x_0..x_2]) ^** 2
assert(toString runLengthEncode(expression \ gens A) == "{x_(0,0)..x_(1,2)}")

-- test of Weyl algebra variable handling
A = QQ[t, dt, WeylAlgebra => {t => dt}]
M = monoid A
assert(A.WeylAlgebra == {{0, 1}})
assert(M.Options.WeylAlgebra == {{M_0, M_1}})
B = first flattenRing(A[t])
assert(gens B == {t_0, t_1, dt})
N = monoid B
assert(B.WeylAlgebra == {{1, 2}})
assert(N.Options.WeylAlgebra == {{N_1, N_2}})

-- test of adjoining variables with local variables
needsPackage "BernsteinSato"
W = QQ[u, v, Du, Dv, WeylAlgebra => {u => Du, v => Dv}];
assert(RatAnn(u^5 - v^2) == ideal{2*u*Du+5*v*Dv+10, 5*u^4*Dv+2*v*Du})

-- test of describe/value
assert(degrees value describe (QQ[x,y,DegreeRank=>2]) === {{1,0},{0,1}})
