newPackage(
	"Betti",
    	Version => "0.1", 
    	Date => "May 5, 2007",
    	Authors => {
	     {Name => "David Eisenbud", Email => "de@msri.org", HomePage => "http://www.msri.org/~de/"},
	     {Name => "Mike Stillman", Email => "mike@math.cornell.edu", HomePage => "http://www.math.cornell.edu/~mike"}
	     },
    	Headline => "betti diagram operations useful for investigating the Boij-Soderberg conjectures",
    	DebuggingMode => true
    	)

export {
     pureBetti, pureBettiDiagram, lowestDegrees, highestDegrees, decomposeBetti, decompose1, scale
     }

--Input: Degs must be a strictly increasing list of positive integers
--Output: List of ranks of the minimal integral betti sequence that satisfy the
--"Peskine-Szpiro" equations
pureBetti = method()
pureBetti List := (Degs) -> (
     c := # Degs;
     p := 1;
     for i from 1 to c-1 do (
	  if Degs#i <= Degs#(i-1) then error "pureBetti: expected an increasing list of integers";
	  for j from 0 to i-1 do p=p*(Degs_i-Degs_j)
	  );
     D := for i from 0 to c-1 list (-1)^i * product(i, j->Degs_j-Degs_i) * product(i+1..c-1, j->Degs_j-Degs_i);
     Bettis := for i from 0 to c-1 list (p/D_i);
     Bettis = Bettis / gcd Bettis;
     apply(Bettis, x -> lift(x,ZZ)))

pureBettiDiagram = method()
pureBettiDiagram List := (degs) -> (
     B := pureBetti degs;
     new BettiTally from apply(#degs, i -> (i, {degs#i}, degs#i) => B#i)
     )

lowestDegrees = method()
lowestDegrees BettiTally := (B) -> (
     pd := pdim B;
     for i from 0 to pd list (
	  B1 := select(keys B, k -> k#0 == i and B#k != 0);
	  min apply(B1, k -> k#2)
	  ))

highestDegrees = method()
highestDegrees BettiTally := (B) -> (
     pd := pdim B;
     for i from 0 to pd list (
	  B1 := select(keys B, k -> k#0 == i and B#k != 0);
	  max apply(B1, k -> k#2)
	  ))

--input: a BettiTally or a similar hash table
--output: a triple, 
--First element: the first summand in the (conjectural) Boij-Soderberg decomposition
--second element: the multiplier
--third element: the result of subtracting it.
decompose1= B->(
     L:=lowestDegrees B;
     C:=pureBettiDiagram L;
     ratio:=min apply(#L, i->(B#(i, {L_i}, L_i))/(C#(i,{L_i},L_i)));
     (C, ratio, B - ratio*C)
     )

scale = method()
scale BettiTally := (B) -> (
     g := gcd values B;
     lift(1/g * B, ZZ)
     )

decomposeBetti = method()
decomposeBetti BettiTally := (B) -> (
     C := B;
     Bs := while min values C >= 0 and max values C > 0 list (
	  (Bi,ri,Cnew) := decompose1 C;
	  C = Cnew;
	  (Bi,ri));
     if any(values C, v -> v < 0)
     then (
	  print "BOIJ-SODERBERG FAILS for this diagram";
	  Bs = append(Bs, scale C);
	  );
     Bs)

randL = (d,b1,b2,R) -> (
     betti res coker random(R^b1, R^{b2:-d})
     )

--randR = (d,b1,b2,R) -> (
--     B := (betti dual res coker random(R^b1, R^{b2:-d}))[-numgens R];
--     d0 := min lowestDegrees B;
--     applyKeys(B, 
--	  k -> (k#0, 
--	       k#1 + {-d0},   -- warning: asssumption here that the degreeLength is 1 (the multi-degree here is k#1)
--	       k#2 - d0)))

doPure = (d) -> (
     b := pureBetti d;
     B := pureBettiDiagram d;
     left := randL(d#1-d#0,b#0,b#1,R);
     right := randR(d#-1-d#-2,b#-1,b#-2,R);
     (d,B,left,right)
     )

doPure2 = (d) -> (
     b := 2 * pureBetti d;
     B := 2 * pureBettiDiagram d;
     left := randL(d#1-d#0,b#0,b#1,R);
     right := randR(d#-1-d#-2,b#-1,b#-2,R);
     (d,B,left,right)
     )

dopure = (d,multiple) -> (
     b := multiple * pureBetti d;
     B := multiple * pureBettiDiagram d;
     left := randL(d#1-d#0,b#0,b#1,R);
     right := randR(d#-1-d#-2,b#-1,b#-2,R);
     (d,B,left,right)
     )

beginDocumentation()

document { Key => Betti,
     Headline => "Betti diagram routines",
     EM "Betti", " is a package designed to help with the investigation of 
     the Boij-Soderberg conjectures.  For the definitions and conjectures, see
     math.AC/0611081, \"Graded Betti numbers of Cohen-Macaulay modules and 
     the Multiplicity conjecture\", by Mats Boij, Jonas Soderberg."
     }

end
restart
loadPackage "Betti"

pureBetti{0,1,2,4}
pureBetti{0,1,2,5}
pureBetti{0,1,2,6}
pureBetti{0,1,2,7}
pureBetti{0,1,3,4}
pureBetti{0,1,3,5}


B1 = pureBettiDiagram{0,1,2,4}
(B2,r,C2) = decompose1 B1

B2 = pureBettiDiagram{0,1,3,6}
3*B1
C = 3 * (1/3 * B1)
lift(C,ZZ)
B1 + B2
B1 - B2
B1
B2
B1 - 2*B2
pureBettiDiagram{0,1,2,5}
pureBettiDiagram{0,1,2,6}
pureBettiDiagram{0,1,2,7}
pureBettiDiagram{0,1,3,4}
B = pureBettiDiagram{0,1,3,5}
lowestDegrees B
highestDegrees B
degree B
pdim B
hilbertSeries(3,B)
poincare B


R = ZZ/32003[a..e]
I = ideal"a2,b3,c3,d4,e7"
B = betti res I
decomposeBetti B
lowestDegrees B
(B1,r1,C1) = decompose1 B
(B2,r2,C2) = decompose1 C1
scale C1
oo_2
4*B

R = ZZ/32003[a,b,c]
I = ideal borel monomialIdeal"a2,ab,c3"
I = ideal borel monomialIdeal"a2,ab,c4"
B = betti res I
decomposeBetti betti res I
I = ideal borel monomialIdeal"b2,bc2,,c4"

degree B
poincare B
codim B
P = hilbertPolynomial(7,B)
1-P(0)

hilbertSeries(5,B)
reduceHilbert oo

R = ZZ/32003[a,b,c]
I = ideal"a3,b3,c3,abc"
B = betti res I
decomposeBetti B
M = coker random(R^2, R^{7:-3})
betti res M

M = coker random(R^8, R^{35:-3});
betti res M

M = coker random(R^3, R^{10:-3});
B = betti res M
decomposeBetti B

M = coker random(R^5, R^{16:-3}); -- pure
M = coker random(R^5, R^{14:-3}); -- pure
M = coker random(R^5, R^{15:-3}); -- not pure
M = coker random(R^3, R^{9:-3});

for d from 31 to 40 list (
     M := coker random(R^5, R^{d:-3});
     betti res M)
scale oo#-1
ooo/scale

for d from 21 to 30 list (
     M := coker random(R^2, R^{d:-3});
     betti res M)

-----------------------------------------
-- Degrees sequences contained in 1..8 --
-----------------------------------------
C1 = apply(subsets({1,2,3,4,5,6,7,8},3), x -> prepend(0,x))
C2 = apply(C1,pureBettiDiagram)

doPure C1#10

L = apply(C1, d -> doPure d)
netList L
L1 = select(L, p -> p#1 != p#2 and p#1 != p#3)
L1 = L1/(x -> toList x)

L2 = apply(L1, x -> doPure2 x#0)
L2 = select(L2, p -> p#1 != p#2 and p#1 != p#3)
L2 = L2/(x -> toList x)
netList L2

L3 = apply(L2, x -> dopure(x#0,4))
L3 = select(L3, p -> p#1 != p#2 and p#1 != p#3)
L3 = L3/(x -> toList x)
netList L3

netList(L3/(x -> scale(x#1)))

              0  1  2 3
o185 = total: 5 32 30 3
           0: 5  .  . .
           1: .  .  . .
           2: . 32 30 .
           3: .  .  . .
           4: .  .  . .
           5: .  .  . 3


R = ZZ/32003[x,y,z]
M1 = random(R^8, R^{6:-1})
M2 = random(R^6, R^{8:-1})
M = M1*(transpose M1)
C = res coker M
res coker transpose C.dd_3
J = ideal(C.dd_3)
res J

M = random(R^2, R^{4:-1})
C = res coker M
C.dd
M = matrix"x,y,z,0;0,x,y,z"
N = mutableMatrix(C.dd_2)
N
rowMult(N,3,-1_R)
columnMult(N,2,-1_R)

-- random skew symmetric matrix
N = mutableMatrix(R,4,4)
N_(0,1) = (first first entries random(R^1, R^{-2}))
N_(1,0) = -N_(0,1)
N
for i from 0 to 2 do for j from i+1 to 3 do (
     f := (first first entries random(R^1, R^{-2}));
     N_(i,j) = f;
     N_(j,i) = -f;
     )
N
N = matrix N
syz N
isHomogeneous N
res coker N
