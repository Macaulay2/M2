-- 0 dim GB

genericPointRing = (R,m) -> (
     -- First create a new ring with correct order
     local ones;
     local mm;
     local F;
     n := numgens R;
     vars1 := support m;
     d := #vars1;
     vars2 := support ((product gens R)//m);
     K := frac((coefficientRing R)[vars1]);
     K[vars2]
     )

getMinimalPoly = (x,I) -> (
     ux := u*x;
     elimvars := select(gens ring I, y -> x % y != 0);
     J := eliminate(elimvars,I);
     if numgens J > 1 then error "expected one minimal polynomial";
     J_0
     )

gb0 = (I, u) -> (
     -- make a new ring with the variables of u
     )

end
restart
load "/Users/mike/M2/Macaulay2/test/radical.m2"
load "1-radical-b.m2"

-- example E2
I = exampleE2(ZZ/2)
  -- this is zero dimensional already
  eliminate({x,y},I)
  eliminate({x,z},I)
  eliminate({y,z},I)

-- example E3
I = exampleE3(ZZ/32003)
dim I
time intersect decompose I;
F = (eliminate({x,y},I))_0
F // gcd(diff(z,F),F)

-- example M
I = exampleM(ZZ/32003)
time intersect decompose I;
u = first independentSets I
R = genericPointRing(ring I,u)
IR = substitute(I,R)
gens gb IR
eliminate({b},IR)
eliminate({c},IR)
flatt(I,u)

b % IR
b^2 % IR
b^3 % IR
b^4 % IR

-- example8'3:
I = example8'3(ZZ/3)
decompose I
intersect oo

I = exampleC(ZZ/5)
time intersect decompose I

-- 3 by 3 nilpotent matrices
R = ZZ/101[vars(0..8)]
M = genericMatrix(R,a,3,3)
I = ideal (M^3)
time J = radical(I, CompleteIntersection=>I1)
assert(J == ideal(trace(M), trace(M^2), trace(M^3)))

time decompose I; -- not good

debug PrimaryDecomposition
time J1 = rad I
first independentSets(I, Limit=>1)
S = genericPointRing(R,oo)
IS = substitute(I,S)
getMinimalPoly(a,IS)
time gens gb IS
vars S
eliminate({d,g},IS)
eliminate({a,g},IS)
eliminate({a,d},IS)
flatt(I,oo_0)
intersect values oo
saturate(I,oo)

top I -- not good

-- Local Variables:
-- M2-send-to-buffer: "*M2*"
-- End:
