newPackage(
	"Schubert",
    	Version => "0.1",
    	Date => "May, 2007",
	Authors => {
	     {Name => "Daniel R. Grayson", Email => "dan@math.uiuc.edu", HomePage => "http://www.math.uiuc.edu/~dan/"},
	     {Name => "Michael E. Stillman", Email => "mike@math.cornell.edu", HomePage => "http://www.math.cornell.edu/People/Faculty/stillman.html"}
	     },
	HomePage => "http://www.math.uiuc.edu/Macaulay2/",
    	Headline => "computations of characteristic classes for varieties without equations",
    	DebuggingMode => true
    	)

export {
     AbstractSheaf,
     AbstractVariety,
     AbstractVarietyMap,
     ChernCharacter,
     ChernClass,
     ChernClassSymbol,
     DIM,
     IntersectionRing,
     TangentBundle,
     ToddClass,
     abstractSheaf,
     abstractVariety,
     adams,
     ch,
     chernClass,
     expp,
     flagVariety,
     integral,
     intersectionRing,
     logg,
     point,
     reciprocal,
     schur,
     segre,
     symm,
     todd,
     wedge
     }

protect TangentBundle
protect ToddClass
protect ChernClass
protect ChernCharacter
protect IntersectionRing

symm = symmetricPower
wedge = exteriorPower

AbstractVariety = new Type of MutableHashTable
AbstractVariety.synonym = "abstract variety"
globalAssignment AbstractVariety
net AbstractVariety := X -> (
     if ReverseDictionary#?X then toString ReverseDictionary#X
     else "an abstract variety")

AbstractVarietyMap = new Type of MutableHashTable
AbstractVarietyMap.synonym = "abstract variety map"

AbstractSheaf = new Type of MutableHashTable
AbstractSheaf.synonym = "abstract sheaf"
globalAssignment AbstractSheaf
net AbstractSheaf := X -> (
     if ReverseDictionary#?X then toString ReverseDictionary#X
     else "an abstract sheaf")
AbstractSheaf.AfterPrint = E -> (
     << endl;				  -- double space
     n := rank E;
     << concatenate(interpreterDepth:"o") << lineNumber << " : "
     << "an abstract sheaf of rank " << rank E << " on " << variety E << endl;
     )

abstractSheaf = method(Options => {
	  ChernClass => null,
	  ChernCharacter => null
	  })
abstractSheaf(AbstractVariety,ZZ) := opts -> (X,rk) -> (
     if opts.ChernClass === null and opts.ChernCharacter === null then error "abstractSheaf: expected Chern character or Chern class";
     new AbstractSheaf from {
     	  global AbstractVariety => X,
     	  global rank => rk,
	  ChernCharacter => if opts.ChernCharacter =!= null then opts.ChernCharacter else rk + logg opts.ChernClass,
	  global cache => new CacheTable from {
	       if opts.ChernClass =!= null then ChernClass => opts.ChernClass
	       }
     	  }
     )
abstractSheaf(AbstractVariety) := opts -> X -> (
     if opts.ChernCharacter === null then error "abstractSheaf: expected rank or Chern character";
     abstractSheaf(X,lift(part(0,opts.ChernCharacter),ZZ),opts))
abstractSheaf(RingElement) := opts -> f -> abstractSheaf((ring f).AbstractVariety, ChernCharacter => f)

abstractVariety = method(Options => {
	  -- TangentBundle => null
	  })
abstractVariety(Ring) := opts -> (A) -> (
     if not A.?DIM then error "intersection ring provided doesn't specify DIM";
     if A.?AbstractVariety then error "intersection ring provided already associated with an abstract variety";
     X := new AbstractVariety from {
	  -- if opts.TangentBundle =!= null then TangentBundle => opts.TangentBundle,
     	  IntersectionRing => A
     	  };
     A.AbstractVariety = X;
     X)
abstractVariety(ZZ,Ring) := opts -> (DIM,A) -> (
     if A.?DIM and A.DIM =!= DIM then error "intersection ring corresponds to a variety of a different dimension";
     A.DIM = DIM;
     abstractVariety(A,opts))
point = abstractVariety(0,QQ)
dim AbstractVariety := X -> X.IntersectionRing.DIM

intersectionRing = method()
intersectionRing AbstractVariety := X -> X.IntersectionRing

c = chernClass = method()
chernClass AbstractSheaf := (cacheValue ChernClass) (F -> expp F.ChernCharacter)
chernClass(ZZ, AbstractSheaf) := (p,F) -> part(p,chernClass F)
chernClass(ZZ, ZZ, AbstractSheaf) := (p,q,F) -> toList apply(p..q, i -> chernClass(i,F))
chernClass(ZZ,Symbol) := (n,E) -> value new ChernClassSymbol from {n,E}

ch = method()
ch AbstractSheaf := (F) -> F.ChernCharacter

chernClassValues = new MutableHashTable
ChernClassSymbol = new Type of BasicList
baseName ChernClassSymbol := identity
installMethod(symbol <-, ChernClassSymbol, (c,x) -> chernClassValues#c = x)
value ChernClassSymbol := c -> if chernClassValues#?c then chernClassValues#c else c
expression ChernClassSymbol := c -> new FunctionApplication from {new Subscript from {symbol c,c#0}, c#1}
net ChernClassSymbol := net @@ expression

OO(AbstractVariety) := X -> (
     A := intersectionRing X;
     abstractSheaf(X,1,ChernClass => 1_A,ChernCharacter => 1_A))

AbstractSheaf ^ ZZ := (E,n) -> new AbstractSheaf from {
     global AbstractVariety => E.AbstractVariety,
     ChernCharacter => n * E.ChernCharacter,
     symbol rank => E.rank * n,
     symbol cache => new CacheTable from {
	  if E.cache.?ChernClass then ChernClass => E.cache.ChernClass ^ n
	  }
     }
rank AbstractSheaf := E -> E.rank
variety AbstractSheaf := E -> E.AbstractVariety

flagVariety = method()
flagVariety(ZZ,List,List) := (rk,bundleNames,bundleRanks) -> flagVariety(point,rk,bundleNames,bundleRanks)
flagVariety(AbstractVariety,ZZ,List,List) := (X,rk,bundleNames,bundleRanks) -> flagVariety(OO_X^rk,bundleNames,bundleRanks)
flagVariety(AbstractSheaf,List,List) := (E,bundleNames,bundleRanks) -> (
     X := E.AbstractVariety;
     n := #bundleRanks;
     if n =!= #bundleNames then error "name list and rank list should have same length";
     if rank E =!= sum bundleRanks then error "expected rank of bundle to equal sum of bundle ranks";
     bundleNames = apply(bundleNames, n -> if ReverseDictionary#?n then ReverseDictionary#n else n);
     vrs := apply(reverse bundleNames, reverse bundleRanks, (E,r) -> apply(reverse toList(1 .. r), i -> new ChernClassSymbol from {i,E}));
     dgs := splice apply(reverse bundleRanks, r -> reverse (1 .. r));
     S := intersectionRing X;
     T := S[flatten vrs, Degrees => dgs, Global => false, MonomialOrder => apply(reverse bundleRanks, n -> RevLex => n)];
     (A,F,G) := flattenRing T;
     chclasses := apply(vrs, x -> F (1 + sum(x,value)));
     rlns := product chclasses - F promote(chernClass E,T);
     rlns = apply(1 .. sum bundleRanks, i -> part_i rlns);
     B := A/rlns;
     (C,H,I) := flattenRing B;
     use C;
     DIM := (intersectionRing X).DIM + sum(n, i -> sum(i+1 .. n-1, j -> bundleRanks#i * bundleRanks#j));
     FV := abstractVariety(DIM,C);
     scan(n, i -> (
	       nm := bundleNames#i;
	       bdl := abstractSheaf(FV,bundleRanks#i, ChernClass => H promote(chclasses#i,B));
	       globalReleaseFunction(nm,value nm);
	       globalAssignFunction(nm,bdl);
	       nm <- bdl;
	       )
	  );
     FV)

Grassmannian(ZZ,AbstractSheaf,List) := opts -> (k,E,bundleNames) -> flagVariety(E,bundleNames,{k,rank E-k})
Grassmannian(ZZ,ZZ,AbstractVariety,List) := opts -> (k,n,X,bundleNames) -> Grassmannian(k,OO_X^n,bundleNames)
Grassmannian(ZZ,ZZ,List) := opts -> (k,n,bundleNames) -> Grassmannian(k,n,point,bundleNames)

Proj(AbstractSheaf,List) := (E,bundleNames) -> Grassmannian(E.rank - 1,E,bundleNames)
Proj(ZZ,AbstractVariety,List) := (n,X,bundleNames) -> Proj(OO_X^(n+1),bundleNames)
Proj(ZZ,List) := (n,bundleNames) -> Proj(OO_point^(n+1),bundleNames)

integral = f -> coefficient((ring f).point, f)		    -- not right, sign could be wrong!

reciprocal = method()
reciprocal RingElement := (A) -> (
     -- computes 1/A (mod degree >=(d+1)
     -- ASSUMPTION: part(0,A) == 1.
     d := (ring A).DIM;
     a := for i from 0 to d list part_i(A);
     recip := new MutableList from splice{d+1:0};
     recip#0 = 1_(ring A);
     for n from 1 to d do
       recip#n = - sum(1..n, i -> a#i * recip#(n-i));
     sum toList recip
     )

logg = method()
logg RingElement := (C) -> (
     -- C is the total chern class in an intersection ring
     -- The chern character of C is returned.
     if not (ring C).?DIM then error "expected a ring with DIM set";
     d := (ring C).DIM;
     p := new MutableList from splice{d+1:0}; -- p#i is (-1)^i * (i-th power sum of chern roots)
     e := for i from 0 to d list part(i,C); -- elem symm functions in the chern roots
     for n from 1 to d do
         p#n = -n*e#n - sum for j from 1 to n-1 list e#j * p#(n-j);
     sum for i from 1 to d list 1/i! * (-1)^i * p#i
     )

expp = method()
expp RingElement := (A) -> (
     -- A is the chern character
     -- the total chern class of A is returned
     if not (ring A).?DIM then error "expected a ring with DIM set";
     d := (ring A).DIM;
     p := for i from 0 to d list (-1)^i * i! * part(i,A);
     e := new MutableList from splice{d+1:0};
     e#0 = 1;
     for n from 1 to d do
	  e#n = - 1/n * sum for j from 1 to n list p#j * e#(n-j);
     sum toList e
     )

todd = method()
todd AbstractSheaf := E -> todd ch E
todd RingElement := (A) -> (
     -- A is the chern character
     -- the (total) todd class is returned
     if not (ring A).?DIM then error "expected a ring with DIM set";
     d := (ring A).DIM;
     -- step 1: find the first part of the Taylor series for t/(1-exp(-t))
     denom := for i from 0 to d list (-1)^i /(i+1)!;
     invdenom := new MutableList from splice{d+1:0};
     invdenom#0 = 1;
     for n from 1 to d do 
       invdenom#n = - sum for i from 1 to n list denom#i * invdenom#(n-i);
     -- step 2.  logg.  This is more complicated than desired.
     R := QQ (monoid[t]);
     R.DIM = d;
     td := logg sum for i from 0 to d list invdenom#i * R_0^i;
     td = for i from 0 to d list coefficient(R_0^i,td);
     -- step 3.  exp
     A1 := sum for i from 0 to d list i! * td#i * part(i,A);
     expp A1
     )

segre = method()
segre AbstractSheaf := E -> reciprocal chernClass dual E
segre(ZZ, AbstractSheaf) := (p,F) -> part(p,segre F)
-- we don't need this one:
-- segre(ZZ, ZZ, AbstractSheaf) := (p,q,F) -> (s := segre F; toList apply(p..q, i -> part(i,s)))

nonnull = x -> select(x, i -> i =!= null)

AbstractSheaf ++ AbstractSheaf :=
AbstractSheaf + AbstractSheaf := (F,G) -> (
     if variety F =!= variety G then error "expected abstract sheaves on the same variety";
     abstractSheaf nonnull (variety F, rank F + rank G, ChernCharacter => ch F + ch G,
	  if F.cache.?ChernClass and G.cache.?ChernClass then ChernClass => F.cache.ChernClass * G.cache.ChernClass
	  ))

adams = method()
adams(ZZ,RingElement) := (k,ch) -> (
     d := first degree ch;
     sum(0 .. d, i -> k^i * part_i ch))
adams(ZZ,AbstractSheaf) := (k,E) -> abstractSheaf(variety E, rank E, ChernCharacter => adams(k, ch E))
dual AbstractSheaf := - AbstractSheaf := E -> adams(-1,E)

AbstractSheaf ** AbstractSheaf :=
AbstractSheaf * AbstractSheaf := (F,G) -> (
     if variety F =!= variety G then error "expected abstract sheaves on the same variety";
     abstractSheaf(variety F, rank F * rank G, ChernCharacter => ch F * ch G))

Hom(AbstractSheaf, AbstractSheaf) := (F,G) -> dual F ** G
End AbstractSheaf := (F) -> Hom(F,F)

det AbstractSheaf := opts -> (F) -> abstractSheaf(variety F, 1, ChernClass => 1 + part(1,ch F))

computeWedges = (n,A) -> (
     -- compute the chern characters of wedge(i,A), for i = 0..n, given a chern character
     wedge := new MutableList from splice{0..n};
     wedge#0 = 1_(ring A);
     wedge#1 = A;
     for p from 2 to n do
	  wedge#p = 1/p * sum for m from 0 to p-1 list (-1)^(p-m+1) * wedge#m * adams(p-m,A);
     toList wedge
     )

exteriorPower(ZZ, AbstractSheaf) := opts -> (n,E) -> (
     -- wedge is an array 0..n of the chern characters of the exerior powers of E.  The last one is what we want.
     if 2*n > rank E then return det(E) ** dual exteriorPower(rank E - n, E);
     wedge := computeWedges(n,ch E);
     abstractSheaf(variety E, ChernCharacter => wedge#n)
     )

symmetricPower(ZZ, AbstractSheaf) := (n,E) -> (
     A := ch E;
     wedge := computeWedges(n,A);
     symms := new MutableList from splice{0..n};
     symms#0 = 1_(ring A);
     symms#1 = A;
     for p from 2 to n do (
	  r := min(p, rank E);
	  symms#p = sum for m from 1 to r list (-1)^(m+1) * wedge#m * symms#(p-m);
	  );
     abstractSheaf(variety E, ChernCharacter => symms#n)
     )

schur = method()
schur(List, AbstractSheaf) := (p,E) -> (
     -- Make sure that p is a monotone descending sequence of non-negative integers
     --q := conjugate new Partition from p;
     q := p;
     n := sum p;
     R := symmRing n;
     wedges := computeWedges(n,ch E);
     J := jacobiTrudi(q,R); -- so the result will be a poly in the wedge powers
     F := map(ring ch E, R, join(apply(splice{0..n-1}, i -> R_i => wedges#(i+1)), 
	                         apply(splice{n..2*n-1}, i -> R_i => 0)));
     ans := F J;
     abstractSheaf(variety E, ChernCharacter => ans)
     )
beginDocumentation()

end

compactMatrixForm = false
loadPackage "Schubert"

Proj(3,{R,Q})
P3 = Proj(3,{R,Q})
A = intersectionRing P3
c_1 R
c_1 Q
c_2 R
c_2 Q
transpose presentation A
basis A

G24 = Grassmannian(2,4,{R,Q})
C = intersectionRing G24
transpose presentation C

F22 = flagVariety(4,{R,Q},{2,2})
A = intersectionRing F22
transpose presentation A
basis A

F222 = flagVariety(6,{P,R,S},{2,2,2})
B = intersectionRing F222
transpose presentation B
transpose basis B
(c_1 P)^3 * (c_1 R)^5 * (c_1 S)^4

-- end of demo

compactMatrixForm = false
loadPackage "Schubert"
A = QQ[e1,e2,Degrees=>{1,2}]
B = A/truncate(4,ideal vars A)
describe B
X = abstractVariety(3,B)
E = abstractSheaf(X,2,ChernClass => 1 + e1 + e2)
c_0 E,c_1 E,c_2 E,c_3 E
P1 = Proj(E,{Q,R})					    -- working on this now ...
C = intersectionRing P1
degree \ gens C					    -- wrong!
describe C
x = c_1 Q
y = c_1 R
x*y							    -- expect e2 ?

-- Mike's demo

loadPackage "Schubert"
R = QQ[c1,c2,c3,c4,Degrees=>{1,2,3,4},MonomialOrder=>GRevLex=>{1,2,3,4}]
X = abstractVariety(4,R)
F = abstractSheaf(X, 4, ChernClass=>1+c1+c2+c3+c4)
segre F
parts oo
chernClass F
parts oo
ch F
parts oo
netList toList oo
a = logg chernClass F
expp a
todd a
parts oo
netList toList oo
det F
schur({2},F)
restart
loadPackage "Schubert"
R = QQ[c3,c2,c1,Degrees=>{3,2,1},MonomialOrder=>RevLex,Global=>false]
A = R/truncate(4,ideal vars R)
X = abstractVariety(3, A)
F = abstractSheaf(X,3,ChernClass => 1+c1+c2+c3)
F2 = wedge(2,F)
F3 = wedge(3,F)
symm(1,F)
symm(2,F)
symm(3,F)
G = F**F**F
H = wedge(20,G)
time symm(20,G)
chernClass H
ch H
chernClass F
segre F
segre(3,F)
parts segre F
netList toList parts segre F

TEST /// -- segre
  loadPackage "Schubert"
  X = abstractVariety(3, use (QQ[c1,c2,c3,Degrees=>{1,2,3},MonomialOrder=>GRevLex=>{1,2,3}]))
  F = abstractSheaf(X,3,ChernClass => 1+c1+c2+c3)
  assert(chernClass F == 1+c1+c2+c3)
  assert(toString segre F == "c1^3-2*c1*c2+c3+c1^2-c2+c1+1")
  assert(segre(3,F) == c1^3-2*c1*c2+c3)
  netList segre(0,3,F)
///

restart
loadPackage "Schubert"
R = QQ[c4,c3,c2,c1,Degrees=>{4,3,2,1},MonomialOrder=>RevLex,Global=>false]
A = R/truncate(10,ideal vars R)
X = abstractVariety(9, A)
F = abstractSheaf(X,4,ChernClass => 1+c1+c2+c3+c4)
F2 = wedge(2,F)
F3 = wedge(3,F)
symm(1,F)
symm(2,F)
symm(3,F)
G = F**F**F
H = wedge(20,G)
time symm(20,G)
chernClass H
ch H
chernClass F
segre F
segre(3,F)
parts segre F
netList toList parts segre F

restart
loadPackage "Schubert"

-- ## # conics on a quintic 3-fold
-- grass(3,5,c,all);
G = Grassmannian(3,5,{Sc,Qc})
AG = intersectionRing G
ch Qc
symm(2,Qc)
netList toList parts chernClass oo
-- Proj(X,dual(symm(2,Qc)),z,f);
-- totalspace(X,Gc);
-- B := symm(5,Qc) - symm(3,Qc) * o(-z);
-- rank(B);
-- chern(11,B);
-- integral(chern(11,B));

-- test wedge, symm for small cases
restart
R = QQ[a,b,c,d,e1,e2,e3,e4,t,MonomialOrder=>Eliminate 4]
I = ideal(e1-(a+b+c+d), e2-(a*b+a*c+a*d+b*c+b*d+c*d), e3-(a*b*c+b*c*d+a*c*d+a*b*d),e4-a*b*c*d)
F = product(1..4, i -> (1+R_(i-1)*t))
F % I
F = product apply(subsets(0..3,2), x -> (1 + (R_(x#0) + R_(x#1)) * t))
F % I
coefficients(oo,Variables=>t)

F = product apply(subsets(0..3,3), x -> (1 + sum(#x, i -> R_(x#i)) * t))
F % I

F = product apply(flatten for i from 0 to 3 list for j from i to 3 list {i,j}, x -> (
	  (1 + sum(#x, i -> R_(x#i))*t)))
F % I

-- Testing schur functions of bundles
restart
loadPackage "Schubert"
R = QQ[c4,c3,c2,c1,Degrees=>{4,3,2,1},MonomialOrder=>RevLex,Global=>false]
A = R/truncate(5,ideal vars R)
X = abstractVariety(4, A)
F = abstractSheaf(X,4,ChernClass => 1+c1+c2+c3+c4)
ch F

-- Check that wedge 2, symm 2, work correctly
S2F = ch symm(2,F)
W2F = ch wedge(2,F)
R = symmRing 4
jacobiTrudi({2},R)
ch wedge(2,F) == (ch F)^2 - ch symm(2,F)
ch wedge(2,F) == (ch F)^2 - ch symm(2,F)
ch schur({2},F) == ch symm(2,F)
ch schur({1,1},F) == ch wedge(2,F)

-- Now let's try 3 parts
debug SchurRings
R = symmRing 4
S = R.Schur
s
s_{1}^3 -- s_3+2*s_(2,1)+s_(1,1,1)
S3 = ch symm(3,F)
W3 = ch wedge(3,F)

ch schur({2,1},F)
L21 = ((ch F)*(ch wedge(2,F)) - W3)
oo == ooo
0 == (ch F)^3 - S3 - W3 - 2*L21
ch wedge(3,F) == ch schur({1,1,1},F)
ch symm(3,F) == ch schur({3},F)
