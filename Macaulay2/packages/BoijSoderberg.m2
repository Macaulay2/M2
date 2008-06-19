newPackage(
	"BoijSoderberg",
    	Version => "0.2", 
    	Date => "June 16, 2008",
    	Authors => {
	     {Name => "David Eisenbud", Email => "de@msri.org", HomePage => "http://www.msri.org/~de/"},
	     {Name => "Mike Stillman", Email => "mike@math.cornell.edu", HomePage => "http://www.math.cornell.edu/~mike"}
	     },
    	Headline => "betti diagram operations useful for investigating the Boij-Soderberg conjectures",
    	DebuggingMode => true
    	)

export {
     mat2betti, -- documented
     lowestDegrees,
     highestDegrees,
     isPure,
     pureBetti, -- documented
     pureBettiDiagram, -- documented

     pureCharFree,
     pureTwoInvariant,
     pureWeyman,
     pureAll,
     
     randomSocleModule,
     randomModule,
     
     pureCohomologyTable, -- documented
     facetEquation, -- documented
     dotProduct
     }
-- Also defined here:
-- pdim BettiTally
-- decompose BettiTally      -- documented
-- matrix BettiTally (2 versions)

-- FIXES A BUG in pdim in M2
--projectiveDimension = B -> max apply ((keys B), i->i_0) 
--     max apply ((keys B), i->i_0);
pdim BettiTally := (B) -> (
     max apply(select(keys B,i->B#i != 0), i -> i#0))

-- input is a matrix over ZZ or QQ
-- output is the corresponding BettiTally 
mat2betti = method()
mat2betti(Matrix,ZZ) := (M,lowDegree) -> (
     e := entries M;
     a := flatten apply(#e, i -> 
	  apply(#e#i, j -> (j, {i+j+lowDegree}, i+j+lowDegree) => if e#i#j != 0 then e#i#j else null));
     a = select(a, b -> b#1 =!= null);
     new BettiTally from a
     )
mat2betti Matrix := (M) -> mat2betti(M,0)

TEST ///
matrix "1,0,0,0;
        0,4,4,1"
mat2betti oo
///

matrix(BettiTally, ZZ, ZZ) := opts -> (B,lowestDegree, highestDegree) -> (
     c := pdim B + 1;
     r := highestDegree - lowestDegree + 1;
     M := mutableMatrix(ZZ,r,c);
     scan(pairs B, (i,v) -> (
	       if v != 0 then
	         M_(i_2-i_0-lowestDegree, i_0) = v;
	       ));
     matrix M
     )

matrix BettiTally := opts -> (B) -> (
     lo := min apply(keys B, i -> if B#i == 0 then infinity else i_2-i_0);
     hi := max apply(keys B, i -> if B#i == 0 then -infinity else i_2-i_0);
     matrix(B,lo,hi)
     )

matrix(BettiTally,ZZ) := opts -> (B,lo) -> (
     hi := max apply(keys B, i -> if B#i == 0 then -infinity else i_2-i_0);
     matrix(B,lo,hi)
     )

TEST ///
R = ZZ/101[a..e]
I = ideal borel monomialIdeal"abc,ad3,e4"
B = betti res I
matrix(B,0,3)
matrix B
B = pureBettiDiagram{0,3,4,7,9}
C = matrix B
assert(B == mat2betti C)

B = pureBettiDiagram{-1,0,3,4,7}
C = matrix B
assert(B == mat2betti(C,-1))
///

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

isPure = method()
isPure BettiTally := (B) -> lowestDegrees B == highestDegrees B

TEST ///
--load "BoijSoderberg.m2"
m=matrix"1,0,0;
0,1,1;
0,0,1"
B=mat2betti m
assert(lowestDegrees B == {0,2,3})
assert(highestDegrees B == {0,2,4})
m=matrix"1,0,0;
0,0,1;
0,0,1"
B=mat2betti m
assert(lowestDegrees B == {0,infinity,3})
assert(highestDegrees B == {0,-infinity,4})
///

-------------------------------------
-- Pure Betti diagrams --------------
-------------------------------------
pureBetti = method()
pureBetti List := (Degs)-> (
--Input: Degs must be a strictly increasing list of positive integers
--Output: List of ranks of the minimal integral betti sequence that satisfy the
--"Peskine-Szpiro" equations
     if not isStrictlyIncreasing Degs then error "--pureBetti was given degrees that were not strictly increasing";
     c:= # Degs;
     p:=1;
     for i from 1 to c-1 do (for j from 0 to i-1 do p=p*(Degs_i-Degs_j));
     D:=for i from 0 to c-1 list(
         (-1)^i* product(i, j->Degs_j-Degs_i)*product(i+1..c-1, j->Degs_j-Degs_i));
     Bettis=for i from 0 to c-1 list (p/D_i);
     Bettis= apply(Bettis/(gcd Bettis), i->lift(i,ZZ)))

--Now some routines for displaying the answer:
--Input: Degs must be a strictly increasing list of positive integers
--Output: Hash table with the same content as the BettiTally that would display
--the Betti numbers given by pureBetti

pureBettiDiagram = method()
pureBettiDiagram List := (Degs) -> (
--The betti diagram of a pure degree sequence (generator of the ray, according
--to Boij-Soderberg; need not be an actual resolution.)
     Bettis:=pureBetti Degs;
     BB:=new MutableHashTable;
     scan(#Degs, i->BB#(i,{Degs_i},Degs_i)=Bettis_i);
     new BettiTally from BB)

--Input: Degs must be a strictly increasing list of positive integers
--Output: List of ranks of the minimal integral betti sequence that satisfy the
--"Peskine-Szpiro" equations
pureBetti = method()
pureBetti List := (Degs) -> (
     c := # Degs;
     p := 1;
     for i from 1 to c-1 do (
	  if Degs#i <= Degs#(i-1) then error "--pureBetti: expected an increasing list of integers";
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

TEST ///
pureBetti{0,1,2,3,4}
B=pureBetti{0,2,3,4}
D1=pureBettiDiagram {0,2,3,4}
///

---------------------------------------------
-- Decoomposing a Betti diagram into pures --
---------------------------------------------

--input: list of rational numbers
--output true if the list is strictly increasing, else false.
isStrictlyIncreasing=L->(
     t:=true;
     for i from 0 to #L-2 do t=(t and (L_i<L_(i+1)));
     t)

TEST ///
debug BoijSoderberg
L={1,4,5,9}
assert(isStrictlyIncreasing L)
L={1,4,5,9,9}
assert(not isStrictlyIncreasing L)
///

--input: a BettiTally or a similar hash table
--output: a triple, 
--First element: the first summand in the (conjectural) Boij-Soderberg decomposition
--second element: the multiplier
--third element: the result of subtracting it.
decompose1= B->(
     L:=lowestDegrees B;
     if not isStrictlyIncreasing L then print "NOT IN THIS SIMPLEX OF PURE BETTI DIAGRAMS";
     C:=pureBettiDiagram L;
     ratio:=min apply(#L, i->(B#(i,{L_i}, L_i))/(C#(i,{L_i},L_i)));
     (C,ratio,merge(B,C, (i,j)->i-ratio*j))
     )

--input: a BettiTally
--output: The routine prints the Boij-Soderberg summands.
--prints "not in convex hull" if the given BettiTally is not in the convex
--hull of the allowable pure betti diagrams. Prints an error message
--if the decomposition fails. Returns a list of the components as a list of pairs,
--each a rational coefficient followed by a hash table representing a pure betti diagram,
--if it succeeds.
decompose BettiTally := B-> (
     Components:={};
     B1:= new MutableHashTable from B;
     while min values B1 >= 0 and max values B1 > 0 do (
	  X:=decompose1(new BettiTally from B1);
	  B1=new MutableHashTable from X_2;
	  --change the type of the values in X_0 to ZZ
	  Y=new BettiTally from apply(pairs X_0, i->{first i, lift(last i, ZZ)});
	  Components = append(Components, hold(X_1) * Y));
     sum Components)

TEST ///
matrix "1,0,0,0;
        0,4,4,1"
mat2betti oo	
decompose oo
///

---------------------------------------------
-- Cohomology Tables ------------------------
---------------------------------------------

pureCohomologyTable = method(TypicalValue => BettiTally)
pureCohomologyTable(List, ZZ, ZZ) := (zeros, lo, hi) -> (
     R := QQ (monoid [z]);
     hp = product(zeros, a -> (R_0 - a));
     n := #zeros;  -- in PP^n
     b := gcd apply(n+1, i -> sub(hp, R_0=>i_QQ));
     hp = hp//b;
     w := 0;
     new BettiTally from for i from lo-n to hi list (
	  v := abs sub(hp, R_0=> i_QQ);
	  if v == 0 then (w=w+1; continue;);
	  d := i+w;
	  (n+i-w, {i},i) => lift(v,ZZ)
	  ));

---------------------------------------------
-- Facet equations and the quadratic form ---
---------------------------------------------
-- The only exported functions here: facetEquation, dotProduct

bettiMatrix = (L,lo,hi) -> matrix(pureBettiDiagram L, lo, hi)

rangeOK=method()
--rangeOK(List,ZZ,ZZ):=(d,r,n)->#d==n+1 and 0<= d_0 and 
--   apply(n,i-> d_i<d_(i+1))==toList(n:true) and d_n<=r+n 
   --tests whether degree seq is non-neg, strictly increasing, of the right length n, and last elt
   -- is bounded by reg + num vars, and 
rangeOK(List,ZZ,ZZ,ZZ):=(d,lowestDegree, highestDegree, n)->
#d==n+1 and lowestDegree<= d_0 and apply(n,i-> d_i<d_(i+1))==toList(n:true) and d_n<=highestDegree+n 
   --tests whether degree seq is >=lowestDegree, strictly increasing, of the right length n, and last elt
   -- is bounded by highestDegree + num vars.

nextLower=method()
nextLower(List, ZZ, ZZ) := (d,lowestDegree, highestDegree) -> (
     --returns A deg seq adjacent to d and below it (chooses one such)
     n:=#d-1;
     if d_0>lowestDegree then join( {d_0-1},d_{1..n}) 
     else if d_0==lowestDegree 
       then (
	    k:=1; 
	    while (d_k-1==d_(k-1) ) do (k=k+1);
	    join(d_{0..k-1},{d_k-1},d_{k+1..n}))
       else error("lowestDegree is too high")
       )

nextUpper=method()
nextUpper(List, ZZ, ZZ):=(d,lowestDegree, highestDegree)->(
	  --same but above
	  n:=#d-1;
	  if d_n<n+highestDegree then join(d_{0..n-1},{d_n+1}) 
	  else if d_n==n+highestDegree then
	  (k:=n; 
	  while (d_k-1==d_(k-1) ) do (k=k-1);
	  join(d_{0..k-2},{d_(k-1)+1},d_{k..n}))
     	  else error"highestDegree is too low")

lowerRange=method()

lowerRange(List,ZZ,ZZ):=(d,lowestDegree, highestDegree)->(
	  --returns a maximal chain of deg seqs below d
	  n:=#d-1;
	  rangeOK(d,lowestDegree, highestDegree,n);
	  A:={d};
	  if d =!= toList(lowestDegree..n+lowestDegree)  then ( 
          e:=nextLower(d,lowestDegree,highestDegree);
	  A=join(A,lowerRange(e,lowestDegree,highestDegree)));
     	  A)

upperRange=method()

upperRange(List,ZZ,ZZ):=(d,lowestDegree, highestDegree)->(
	  --returns a maximal chain of deg seqs below d
	  n:=#d-1;
	  rangeOK(d,lowestDegree, highestDegree,n);
	  A:={d};
	  if d =!= toList(highestDegree..n+highestDegree) then ( 
          e:=nextUpper(d,lowestDegree,highestDegree);
	  A=join(A,upperRange(e,lowestDegree,highestDegree))); --EEE
     	  A)

rangeMatrices=method()
rangeMatrices(List,ZZ,ZZ):=(e,lowestDegree,highestDegree)->( 
	       --takes a deg seq, returns list of mats, k-th has 
	       --a one in posn e_k-k-lowestDegree, k
	       n:=#e-1;
	       r:=highestDegree-lowestDegree;
               apply(n+1,k->
		    map(ZZ^(r+1), ZZ^(n+1),
			 (i,j)-> if i==e_k-k-lowestDegree and j==k then 1 else 0)  -- subtracted lowestDegree
		    )
	       )


peskineSzpiro=(r,n)->apply(n+r+1,k->matrix apply(r+1,i->apply(n+1,j->
--returns (redundant) list of n+r+1 Peskine-Szpiro relations in hilb function form
--(the Hilb fcn values from *** to ***) Note: the P-S eqns in this sense are
--\chi(O(m)\otimes M)= 0 .
	       (if k<r then 1 else (-1)^(k-r))*(-1)^(n-j)*
	       binomial(n+r-k-i-j+n-1,n-1)
	       )))
--PS=peskineSzpiro(r,n); 
--This was a global variable used in numericalComplex and flipEquation only

numericalComplex=A->(
     n:=rank source A-1;
     r:=rank target A-1;
     AA:=A;
     apply(n+r+1,i->(
	       PS:=peskineSzpiro(r,n); 
	       ss:=if i<=r then AA_(r-i,n) else AA_(0,n-i+r);
	       AA=AA-ss*PS_i;
	       ss)))
--The numerical complex that flips the "upper" eqn to the "lower" eqn, written
--as the sequence of coefficients of the PS-equations.

flipEquation=(A)->(
     n:=rank source A-1;
     r:=rank target A-1;
     aa:=numericalComplex(A);
     PS:=peskineSzpiro(r,n); 
     apply(n+r+1,c->A-sum(c,i->aa_(i)*PS_(i))))

upperEquation=(A)->(L:=flipEquation(A);L_(#L-1))
--the necessary lin comb of the PS equations.

///
restart
load "BoijSoderberg.m2"
d={0,1,4}
F=facetEquation(d,0,0,3)
numericalComplex F
flipEquation F
upperEquation F

d={0,3,4,5,8,10}
F=facetEquation(d,2,-3,10)
numericalComplex F
flipEquation F
upperEquation F

--Note: UpperEquation has zeros above and ON the places where the betti diagram
--for the degree sequence d is nonzero. But this fact follows from the vanishing of
--the lower equation at such sequences.
///


middleComplex=(d,e)->(
     n=#d-1;
     t:=1;
     L:=apply(n+1,i->(t=t*if d_i==e_i then 1 else -1 ));
     apply(n+1,c->if L_c==1 then e_c else d_c))

nextPure=(d,k)->if d_k+1==d_(k+1) and (k==#d-2 or d_(k+2)>d_(k+1)+1) then 
     apply(#d,i->if i<k or i>( k+1) then d_i else d_i+1) else error("no next pure sequence")
--in the case of two degree sequences differing by one in two consec places, computes
--the degree sequence in between.



facetEquation=method();

facetEquation(List,ZZ,ZZ,ZZ):=(d,i,lowestDegree, highestDegree)->(
     --i an integer between 0 and #d-2
     --d a strictly increasing list of integers 
     --such that d_(i+1)=d_i+1
     --lowestDegree < highestdegree, integers 
     --lowest degree should be <=d_0, highestDegree-lowestDegree >=d_n-n, and >=d_n-n+2 when i=n-1,
     --where n=#d-1.
--routine produces the "upper" equation of the supporting hyperplane
--of the facet corresponding to d< (...d_i+1, d_(i+1)+1,...)=nextpure(d,k).
--the equation is presented as a 
-- map ZZ^(#d) --> ZZ^(highest-lowest+1).
     n:=#d-1;
     rangeOK(d,lowestDegree,highestDegree,n);
     if d_i+1<d_(i+1) then error("innapropriate value of i");
     e:=nextPure(d,i);
     A:=matrix apply(lowerRange(d,lowestDegree, highestDegree),
	  c->join(toSequence entries bettiMatrix(c,lowestDegree,highestDegree)));
     B:=matrix apply(upperRange(e,lowestDegree,highestDegree),
	  c->join(toSequence entries bettiMatrix(c,lowestDegree,highestDegree)));
     C:=matrix apply(rangeMatrices(e,lowestDegree,highestDegree),c->join(toSequence entries c));
     D:=(entries transpose syz(A||B||C))_0;
     F:=matrix apply(highestDegree-lowestDegree+1,i->apply(n+1,j->D_((n+1)*i+j)));
     de:=middleComplex(d,e);
     B1:=bettiMatrix(de,lowestDegree,highestDegree);
     if dotProduct(F,B1)>0 then F else -F)


///
restart
load "BoijSoderberg.m2"
d={0,1,4}
e=nextPure(d,0)
facetEquation(d,0,-1,3)

d={0,3,5,6}
facetEquation(d,2,-3,4) -- OK
facetEquation(d,2,0,3) -- gives error msg.

d={0,1,3}
facetEquation(d,0,0,0)
--gives error msg

d={0,1,3}
facetEquation(d,0,-1,1)
facetEquation(d,0,0,2)
d={0,2,3}
facetEquation(d,1,-2,2)



d={1,3,4,5,7}
facetEquation(d,2,1,3)
bettiH pureBettiDiagram d
d={5,7,8,11}
facetEquation(d,1,5,8)
facetEquation(d,1,0,8)
d={5,7,9,10}
///

dotProduct=method()

dotProduct(Matrix, Matrix):=(A,B)->
--dot product of matrices as vectors
     sum(rank target A,i->sum(rank source A,j->A_(i,j)*B_(i,j)))

dotProduct(HashTable, HashTable):=(A,B)-> --Gets in the way of another method
     --dot product of two hash tables with similar structure
     sum(keys A, k->(if B#?k then return (B#k) * (A#k) else return 0))

dotProduct(Matrix, ZZ, BettiTally):=(A,lowest, B)->(
     --lowest is an integer specifying what degree the first row of A is supposed to have.
     nr=rank target A;
     highest=nr+lowest-1;
     nc=rank source A;
     d0=min((keys B)/last);
     regB=max(((keys B)/last)-(keys B)/first);
     lengthB=max((keys B)/first);
     if d0<lowest then error "matrix A begins in too high a degree";
     if regB>highest then error "matrix A stops in too low a degree";
     if nc < 1+lengthB then error "matrix A has too few columns";
     sum(keys B, k-> (B#k)*(A_(last k-first k-lowest, first k)))
     )


supportFunctional=method()

supportFunctional(ChainComplex, ChainComplex):=(E,F)->(
     --E should be a chain complex starting in degree 0 and going to negative degrees.
     --F should be a chain complex starting in a postive degree and going to degree 0
     -- the code is meant to execute 
     --\langle E, \beta\rangle = \sum_{j\leq i}(-1)^{i-j}\sum_k\beta_{i,k}h_{-k}(H^j(E)),
     --
lengthF=length F;
degreesF=flatten (for i from 0 to lengthF list flatten degrees F_i);
minF = min degreesF;
maxF = max degreesF;
HHE=HH E;
L=for i from 0 to length E list matrix{{hf(minF..maxF, (HH E)#(-i))}};
A=transpose L_0;
for i from 1 to length L -1 do A = A|(transpose L_i);
AA=map(ZZ^(maxF-minF-lengthF+1), ZZ^(lengthF+1), (p,q)->
     sum(0..min(q,length E), 
	  j->if HHE#?(-j) then (-1)^(q-j)*hilbertFunction(-p-q, (HHE)#(-j)) else 0));
dotProduct(AA, minF, betti F)
     )

supportFunctional(ChainComplex, BettiTally):=(E,B)->(
     --E should be a chain complex starting in degree 0 and going to negative degrees.
     --F should be a chain complex starting in a postive degree and going to degree 0
     -- the code is meant to execute 
     --\langle E, \beta\rangle = \sum_{j\leq i}(-1)^{i-j}\sum_k\beta_{i,k}h_{-k}(H^j(E)),
     --
lengthF=max apply(keys B, K->first K);
degreesF=apply(keys B, K->last K);
minF = min degreesF;
maxF = max degreesF;
HHE=HH E;
L=for i from 0 to length E list matrix{{hf(minF..maxF, (HH E)#(-i))}};
A=transpose L_0;
for i from 1 to length L -1 do A = A|(transpose L_i);
AA=map(ZZ^(maxF-minF-lengthF+1), ZZ^(lengthF+1), (p,q)->
     sum(0..min(q,length E), 
	  j->if HHE#?(-j) then (-1)^(q-j)*hilbertFunction(-p-q, (HHE)#(-j)) else 0));
dotProduct(AA, minF, B)
     )

---------------------------------------------
-- Pure Betti diagrams that actually occur --
---------------------------------------------
rkSchur = (n,L) -> (
--rank of a Schur functor on a module
--input: a non-neg integer n and a non-increasing sequence of non-neg integers L
--output: the rank of the representation S_L(C^n). Here (11111) represents an exterior power, (k) a symmetrci power.
     M:=L;
     if #M<n then M=L|toList(n-#M:0);
     det map(ZZ^n, ZZ^n, (i,j)->binomial(M_i+n-1-i+j, n-1)))

TEST ///
rkSchur(6,{1,1,1,1}) -- exterior power
rkSchur(6,{2}) -- symmetric power
rkSchur(3,{3,2,0})
rkSchur(3,{4,3,1}) -- the previous tensored with the top exterior power

R = schurRing(s,6)
assert(dim s_{1,1,1,1} == rkSchur(6,{1,1,1,1})) -- exterior power
assert(dim s_{2} == rkSchur(6,{2})) -- symmetric power
R = schurRing(s,3)
assert(dim s_{3,2,0} == rkSchur(3,{3,2,0}))
assert(dim s_{4,3,1} == rkSchur(3,{4,3,1})) -- the previous tensored with the top exterior power
///

pureCharFree = method()
pureCharFree List := (L) -> product(1..#L-1, i->binomial(L_i-L_0-1, L_i-L_(i-1)-1))
--gives beta_0 for the pure complex described in Eisenbud-Schreyer

pureTwoInvariant = method()
pureTwoInvariant List := (L) -> (
--gives beta_0 for the pure complex described in Eisenbud-Weyman-Floeystad for two modules
   n:=#L-1;
   T={};
     E=for i from 1 to #L-1 list (L_i-L_(i-1));
     for i from 1 to #E do
     for j from 1 to E_(n-i)-1 do (T= T| {n-i});
     T=T |  {0};
     rkSchur(#T, toSequence T)
     )

pureWeyman = method()
pureWeyman List := (L) -> (
     D:=for i from 1 to #L-1 list L_i-L_0;
     E0:=for i from 1 to #D-1 list  D_(i)-D_(i-1)-1;
     E:={D_0-1}|E0;
     Eplus1:=E+toList(#D:1);
     lambda = for i from 1 to #D list sum E_{i..#D-1};
     rkSchur(#D,lambda)
     )

pureAll = method()
pureAll List := (L) -> (pureCharFree L, pureTwoInvariant L, pureWeyman L)

TEST ///
pureAll{0,1,2,3,4}
pureAll{0,1,3,4}
W = pureAll{0,4,6,9,11}
P = pureBetti{0,4,6,9,11}
for i from 0 to #W-1 list (W_i/P_0)
///

----------------------------------------------------
-- Constructions often producing pure resolutions --
----------------------------------------------------

--Given a strictly increasing degree sequence L and a number of gneerators m,
--this routine produces a "generic" module of finite length with the 
--m generators and number of socle elements  and regularity corresponding
--to the pure resolution with degree sequence L. The module is constructed
--by taking a certain number of generic elements inside an appropriate direct
--sum of copies of a zero-dimensional complete intersection. We use the fact
--that in a polynomial ring in c variables, 
--modulo the r+1 st power of each variable, the part of
--generated in degree (c-1)r looks like the part of the injective hull
--of the residue class field generated in degree -r.

randomSocleModule = method(Options => {CoefficientRing => ZZ/101})
randomSocleModule(List, ZZ) := opts -> (L, m) -> (
     c:=#L-1; -- codimension
     r:=last L-first L-c; -- regularity
     s:=c*r; -- socle degree mod (vars)^[r+1]
     kk := opts.CoefficientRing;
     R:=kk[vars(0..c-1)];
     mR := ideal apply(gens R, x -> x^(r+1));
     B:=pureBetti L;
     f:=random(R^(m*B_c), R^{m*B_0:-s+r});
     prune (image (f**(R^{s-r}/mR)))
     )
     
TEST ///
L={0,1,3,4}
B = pureBettiDiagram L
betti res randomSocleModule(L,1)
assert(2*B == betti res randomSocleModule(L,2))
assert(3*B == betti res randomSocleModule(L,3))

L={0,2,3,6}
B = pureBettiDiagram L
assert(B == betti res randomSocleModule(L,1))
assert(2*B == betti res randomSocleModule(L,2))
assert(3*B == betti res randomSocleModule(L,3))
///

randomModule = method(Options => {CoefficientRing => ZZ/101})
randomModule(List,ZZ) := opts -> (L, m) -> (
     c:=#L-1; -- codimension
     kk := opts.CoefficientRing;
     R:=kk[vars(0..c-1)];
     B:=pureBetti L;
     coker (M=random(R^{m*B_0:-L_0}, R^{m*B_1:-L_1})))

TEST ///
L={0,4,9,10}
B = pureBetti L
betti res randomModule(L,1)
betti res randomModule(L,2)
betti res randomModule(L,3)
betti res randomModule(L,4)
betti res randomModule(L,2, CoefficientRing=>ZZ/5)
///

beginDocumentation()

document { Key => BoijSoderberg,
     Headline => "Betti diagram routines",
     EM "BoijSoderberg", " is a package designed to help with the investigation of 
     the Boij-Soderberg conjectures and theorems.  For the definitions and conjectures, see
     math.AC/0611081, \"Graded Betti numbers of Cohen-Macaulay modules and 
     the Multiplicity conjecture\", by Mats Boij, Jonas Soderberg."
     }

document { 
     Key => {(pureBetti,List),pureBetti},
     Headline => "",
     Usage => "pureBetti L",
     Inputs => {
	  "L" => "of strictly increasing integers"
	  },
     Outputs => {
	  List => "a list of the minimal integral Betti numbers which satisfy the Herzog-Kuhl equations"
	  },
     "The numerator P(t) of the Hilbert function of a module whose free resolution has a pure resolution
     of type L has the form P(t) = b_0 t^(d_0) - b_1 t^(d_1) + ... + (-1)^c b_c t^(d_c), 
     where L = {d_0, ..., d_c}.  If (1-t)^c divides P(t), as in the case where the module has codimension c,
     then the b_0, ..., b_c are determined up to a unique scalar multiple.  This 
     routine returns the smallest positive integral solution of these (Herzog-Kuhl) equations.",
     EXAMPLE lines ///
     	  pureBetti{0,2,4,5}
	  pureBetti{0,3,4,5,6,7,10}
	  ///,
     Caveat => {},
     SeeAlso => {pureBettiDiagram}
     }

document { 
     Key => {(pureBettiDiagram,List),pureBettiDiagram},
     Headline => "",
     Usage => "pureBettiDiagram L",
     Inputs => {
	  "L" => "of strictly increasing integers"
	  },
     Outputs => {
	  BettiTally => "containing the minimal integral Betti numbers which satisfy the Herzog-Kuhl equations"
	  },
     "See ", TO "pureBetti", " for a description of the Herzog-Kuhl equations.",
     EXAMPLE lines ///
     	  pureBettiDiagram{0,2,4,5}
	  pureBettiDiagram{0,3,4,5,6,7,10}
	  pureBettiDiagram{0,3,4,5,6,7,8,11}
	  ///,
     Caveat => {},
     SeeAlso => {pureBetti, betti}
     }

document { 
     Key => (decompose,BettiTally),
     Headline => "write a Betti diagram as a positive combination of pure integral diagrams",
     Usage => "decompose B",
     Inputs => {
	  "B" => "not necessarily Cohen-Macaulay"
	  },
     Outputs => {
	  Expression => "a positive combination of pure integral Betti diagrams"
	  },
     EXAMPLE lines ///
     	  R = ZZ/103[a,b,c]
	  I = ideal"a3,abc,b4,c4,b2c2"
	  B = betti res I
	  C = decompose B
	  ///,
     "Check that this really does sum to B:",
     EXAMPLE lines ///
     	  value C
     	  ///,
     "Note that the entries are displayed in a peculiar manner.  Let's lift this to the integers.",
     EXAMPLE lines ///
     	  lift(value C, ZZ)
	  B == oo
     	  ///,
     "Let's display the list of Betti diagrams in the decomposition, and also the list of multipliers.",
     EXAMPLE lines ///
     	  netList pack(3, apply(toList C, x -> x#1))
	  apply(toList C, first)
     	  ///,
     Caveat => {},
     SeeAlso => {pureBettiDiagram, betti, value, lift, toList, pack}
     }

document { 
     Key => {(mat2betti,Matrix,ZZ),(mat2betti,Matrix),mat2betti},
     Headline => "matrix to Betti diagram",
     Usage => "mat2betti(M,lowDegree)\nmat2betti M",
     Inputs => {
	  "M" => "A matrix of Betti numbers: each row corresponds to a slanted degree",
	  "lowDegree" => ZZ => "default value is 0"
	  },
     Outputs => {
	  BettiTally
	  },
     "Change from matrix representation to Betti diagram representation.",
     EXAMPLE lines ///
     	  M = matrix"1,0,0,0;0,3,0,0;0,0,5,3"
	  mat2betti M
	  mat2betti(M,3)
	  ///,
     SeeAlso => {(matrix,BettiTally)}
     }

document { 
     Key => {(matrix,BettiTally,ZZ,ZZ),(matrix,BettiTally,ZZ),(matrix,BettiTally)},
     Headline => "Betti diagram to matrix",
     Usage => "matrix B\nmatrix(B,lowDegree)\nmatrix(B,lowDegree,highDegree)",
     Inputs => {
	  "B" => BettiTally,
	  "lowDegree" => ZZ,
	  "highDegree" => ZZ
	  },
     Outputs => {
	  Matrix => "The Betti diagram as a matrix"
	  },
     "If either lowDegree or highDegree is not given, then they are inferred from the Betti
     diagram itself.  The result matrix has highDegree-lowDegree+1 rows, 
     corresponding to these (slanted) degrees. ",
     EXAMPLE lines ///
     	  B = pureBettiDiagram {0,1,4,7}
	  matrix B
	  matrix(B,-2)
	  matrix(B,-2,5)
	  ///,
     "This function is essentially the inverse of ", TO "mat2betti", ".",
     EXAMPLE lines ///
         R = ZZ/101[a..e];
     	 I = ideal borel monomialIdeal"abc,ad3,e4";
     	 B = betti res I
     	 C = matrix B
     	 B == mat2betti C
	 ///,
     "If the lowest degree of the matrix is not 0, then this information must be
     supplied in order to obtain the inverse operation.",
     EXAMPLE lines ///
     	  B = pureBettiDiagram {-2,0,1,2,5}
	  C = matrix B
	  mat2betti(C,-2)
     	  ///,
     Caveat => {"Currently, the error messages are not that illuminating.  
	  The [lowDegree, highDegree], if given, must be 
	  as large as the actual degree range"},
     SeeAlso => {mat2betti, lowestDegrees, highestDegrees, isPure, pureBettiDiagram}
     }

document {
	Key => {(pureCohomologyTable,List,ZZ,ZZ),pureCohomologyTable},
	Headline => "pure cohomology table given zeros of Hilbert polynomial",
	Usage => "pureCohomologyTable(L,lo,hi)",
	Inputs => { "L" => " a list of distinct integers",
	     "lo" => "the leftmost degree of the table",
	     "hi" => "the rightmost degree of the table"
	     },
	Outputs => {{"the cohomology table, truncated at the given degrees "}},
	EXAMPLE lines ///
	   B = pureCohomologyTable({-3,-2,0},-10,10)
     	///
	}

document {
	Key => {(facetEquation,List,ZZ,ZZ,ZZ),facetEquation},
	Headline => "The upper facet equation corresponding to (L,i)",
	Usage => "facetEquation(L,i,lodeg,hideg",
	Inputs => { "L" => "the degree sequence, a strictly ascending list of integers",
	     "i" => "an index in range 0..#L-2, such that L_(i+1) == L_i + 1",
	     "lo" => "the leftmost degree of the table",
	     "hi" => "the rightmost degree of the table"
	     },
	Outputs => {{"A matrix whose rows correspond to slanted degrees lodeg .. hideg, such that the
		  dot product of this matrix with any betti diagram of any finite length module
		  is >= 0."}},
	EXAMPLE lines ///
	   d = {0,2,3,6,7,9}
	   B1 = pureBettiDiagram d
	   B2 = pureBettiDiagram {0,2,4,6,7,9}
	   B3 = pureBettiDiagram {0,3,4,6,7,9}
	   C = facetEquation(d,1,0,6)
     	   dotProduct(B3,C)
     	///,
	"The following example is from Eisenbud and Schreyer,
	math.AC/0712.1843v2, example 2.4.  Notice that the notation here
	differs from their description of the facet equation.  Our d refers to
	their d-, and the indexing in Macaulay2 is from 0, hence i=3 and not 4,
	as in the example in the paper.",
	EXAMPLE lines ///
	   d = {-4,-3,0,2,3,6,7,9}
	   de = {-4,-3,0,2,4,6,7,9}
	   e = {-4,-3,0,3,4,6,7,9}
	   pureBettiDiagram d
	   pureBettiDiagram de
	   C = facetEquation(d,3,-6,3)
	///,
	"Let's check that this is zero on the appropriate pure diagrams, and positive on 
	the one corresponding to de:",
	EXAMPLE lines ///
	   dotProduct(C,-6,pureBettiDiagram d)
	   dotProduct(C,-6,pureBettiDiagram e)
	   dotProduct(C,-6,pureBettiDiagram de)
	///,
	SeeAlso => {dotProduct, pureBettiDiagram}
	}

TEST ///
d={0,1,4}
facetEquation(d,0,-1,3)

d={0,3,5,6}
facetEquation(d,2,-3,4) -- OK
facetEquation(d,2,0,3) -- gives error msg.

d={0,1,3}
facetEquation(d,0,0,0)
--gives error msg

d={0,1,3}
facetEquation(d,0,-1,1)
facetEquation(d,0,0,2)
d={0,2,3}
facetEquation(d,1,-2,2)



d={1,3,4,5,7}
facetEquation(d,2,1,3)
pureBettiDiagram d
d={5,7,8,11}
facetEquation(d,1,5,8)
facetEquation(d,1,0,8)
d={5,7,9,10}
facetEquation(d,2,0,12)
///

TEST ///
debug BoijSoderberg
d={1,3,4,5,7}
e={1,3,5,6,7}
de = {1,3,4,6,7}
B = pureBettiDiagram d
Be = pureBettiDiagram e
Bde = pureBettiDiagram de
A=facetEquation(d,2,1,3)
assert(dotProduct(A,1,B) == 0)
assert(dotProduct(A,1,Be) == 0)
assert(dotProduct(A,1,Bde) == 180)

A=facetEquation(d,2,-5,3)
assert(dotProduct(A,-5,B) == 0)
assert(dotProduct(A,-5,Be) == 0)
assert(dotProduct(A,-5,Bde) == 180)
///

end

restart
loadPackage "BoijSoderberg"
installPackage "BoijSoderberg"
viewHelp BoijSoderberg
viewHelp pureBetti

M = matrix "1,0,0,0;
        0,4,4,1"
mat2betti M
mat2betti(M, -2)
isPure oo
decompose oo

debug BoijSoderberg
d = {0,2,3,6}
pureBettiDiagram d
lowerRange(d, 0,3)
upperRange(d,0,3)
facetEquation(d,1,0,3)

restart
--load "/Users/mike/local/conferences/2008-mrc-june-snowbird/boij-soderberg/boijSoderberg-orig.m2"
load "/Users/mike/local/conferences/2008-mrc-june-snowbird/boij-soderberg/bs.m2"
matrix "1,0,0,0;
        0,4,4,1"
mat2betti oo	
decomposeAll oo

F=facetEquation({-1,0,2,3},1,-10,10)

document { 
     Key => {},
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Outputs => {
	  },
     EXAMPLE lines ///
	  ///,
     Caveat => {},
     SeeAlso => {}
     }

document { 
     Key => {},
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Outputs => {
	  },
     EXAMPLE lines ///
	  ///,
     Caveat => {},
     SeeAlso => {}
     }

