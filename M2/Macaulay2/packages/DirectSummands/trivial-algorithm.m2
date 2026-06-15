needsPackage "RationalPoints2"
debug needsPackage "DirectSummands"

List * Set := List => (x,y) -> select(x, i -> y#?i)

idempotentsFromElimination = M -> (
    R := ring M;
    F := groundField R;
    B := gensEnd0 M;
    -- TODO: can we reduce all to the residue field and solve there?
    H := apply(numcols B, i -> homomorphism B_{i}) - set { id_M, 0 * id_M };
    T := R(monoid[Variables => #H]);
    f := map(T, R);
    A := sum(gens T, H, (g, h) -> g * (f ** h));
    I := trim ideal last coefficients(cover(A^2 - A),
	Variables => first entries f(vars R));
    -- decompose I
    T' := F(monoid T);
    f' := map(T', T);
    pts := rationalPoints f'(I);
    -- if #pts == 2 then module is indecomposable
    idems := apply(pts, pt -> sum(H, pt, times)) - set { id_M, 0 * id_M };
    G := mingens sum(idems, image @@ homomorphism');
    idems * set apply(numcols G, i -> homomorphism inducedMap(target B, , G_{i}))
)

summandsFromElimination = M -> prune \ image \ idempotentsFromElimination M

end--
restart
needs "DirectSummands/elimination.m2"

S = (ZZ/2)[x]
M = S^1 ++ S^1/(x^2)
elapsedTime summandsFromElimination M
elapsedTime summands M
idempotentsFromElimination M

S = (ZZ/2)[x,y,z]
I = ideal(x^3+y^3+z^3)
R = S/I
X = Proj R
M = module frobeniusPushforward(1, R)
elapsedTime summandsFromElimination M -- faster
elapsedTime summands M

S = (ZZ/5)[x,y,z,w]
I = ideal(x^3+y^3+z^3+w^3)
R = S/I
X = Proj R
M = module frobeniusPushforward(1, OO_X)
elapsedTime summandsFromElimination M -- doesn't finish
elapsedTime summands M

K = QQ
R = K[a,b,c,d];
M = coker matrix"a,b,c,d;d,a,b,c;c,d,a,b;b,c,d,a"
elapsedTime summandsFromElimination M -- doesn't work
elapsedTime summands M

-- FIXME: rationalPoints doesn't work over this field
K = toField(QQ[i]/(i^2+1));
S = K[x,y,z];
M = coker matrix matrix"1,y,z;y,1,x;z,x,1"
elapsedTime summandsFromElimination M
elapsedTime summands M


--- initial attempt
-- TODO: delete

T := R(monoid[]);
use source psi
A=sub(cover sum for i to numcols B - 1 list a_i * homomorphism B_(i-1), Ra)
J=trim ideal cover(A^2-A)
degree eliminate(apply(gens R,i->sub(i,ambient Ra)),  sub(J,ambient Ra)+ideal(Ra))
decompose J

---
ungraded case:
d = numgens End M0
(Ra,psi) = flattenRing( R[a_1..a_d])
M=M0**source psi
E=End M
use source psi
A=sub(cover sum for i from 1 to d list a_i * homomorphism E_(i-1),Ra)
J=trim ideal cover(A^2-A)
eliminate(apply(gens R,i->sub(i,ambient Ra)),  sub(J,ambient Ra)+ideal(Ra))
decompose (J+ideal(x,y,z))

----
S=(ZZ/2)[x,y,z]
I=ideal(x^3+y^3+z^3)
R=S/I
X=Proj R
M0 = module frobeniusPushforward(1,R)
R=ring M0
d = rank End sheaf M0
(Ra,psi) = flattenRing( R[a_1..a_d])
M=M0**source psi
B=smartBasis({0,0},End( M, DegreeLimit=>0))
use source psi
A=sub(cover sum for i from 1 to rank source B list a_i * homomorphism B_(i-1),Ra)
J=trim ideal cover(A^2-A)
degree eliminate(apply(gens R,i->sub(i,ambient Ra)),  sub(J,ambient Ra)+ideal(Ra))
netList decompose J

restart
debug needsPackage "DirectSummands"
needsPackage "RationalPoints2"

S = (ZZ/2)[x]
M0 = S^1 ++ S^1/(x^2)
d = numcols basis(0, End M0)
(S', psi) = flattenRing( S[a_0..a_(d-1)])
M = M0 ** source psi
B = smartBasis({0}, End( M0, DegreeLimit => 0))

A = sum(numcols B, i -> a_i * map(M ** S', M ** S', psi cover homomorphism B_i))
J = trim ideal cover(A^2 - A)
netList decompose J
I = first decompose J
syz gens I



S=(ZZ/5)[x,y,z,w]
I=ideal(x^3+y^3+z^3+w^3)
R=S/I
X=Proj R
M0 = module frobeniusPushforward(1,OO_X)
d = rank End sheaf M0
(Ra,psi) = flattenRing( R[a_1..a_d])
M=M0**source psi
B=smartBasis({0,0}, End(M, DegreeLimit=>0))
use source psi
A=sub(cover sum for i from 1 to rank source B list a_i * homomorphism B_(i-1),Ra)
J=trim ideal cover(A^2-A)
use ambient Ra
degree eliminate(apply(gens R,i->sub(i,ambient Ra)),  sub(J,ambient Ra)+ideal(Ra))

summands M0

K = QQ
R = K[a,b,c,d];
M0=coker matrix"a,b,c,d;d,a,b,c;c,d,a,b;b,c,d,a"
r = rank source basis(0, End M0)
(Ra,psi) = flattenRing( R[aa_1..aa_r])
M=M0**source psi
B=smartBasis({0,0},End( M, DegreeLimit=>0))
use source psi
A=sub(cover sum for i from 1 to rank source B list aa_i * homomorphism B_(i-1),Ra)
J=trim ideal cover(A^2-A)
use ambient Ra
rationalPoints(sub(J,ambient Ra)+ideal Ra+ideal( apply(gens R,i->sub(i,ambient Ra))))
decompose(J+ideal(x,y,z,w))

summands M0
