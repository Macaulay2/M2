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
     else "--an abstract variety--")

AbstractVarietyMap = new Type of MutableHashTable
AbstractVarietyMap.synonym = "abstract variety map"

AbstractSheaf = new Type of MutableHashTable
AbstractSheaf.synonym = "abstract sheaf"

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

abstractVariety = method(Options => {
	  -- TangentBundle => null
	  })
abstractVariety(Ring) := opts -> (A) -> (
     if not A.?DIM then error "intersection ring provided doesn't specify DIM";
     new AbstractVariety from {
	  -- if opts.TangentBundle =!= null then TangentBundle => opts.TangentBundle,
     	  IntersectionRing => A
     	  }
     )
abstractVariety(ZZ,Ring) := opts -> (DIM,A) -> (
     if A.?DIM and A.DIM =!= DIM then error "intersection ring corresponds to a variety of a different dimension";
     A.DIM = DIM;
     abstractVariety(A,opts))
point = abstractVariety(0,QQ)
dim AbstractVariety := X -> X.IntersectionRing.DIM

intersectionRing = method()
intersectionRing AbstractVariety := X -> X.IntersectionRing

chernClass = method()
chernClass AbstractSheaf := (cacheValue ChernClass) (F -> expp F.ChernCharacter)

ch = method()
ch AbstractSheaf := (F) -> F.ChernCharacter

chernClassValues = new MutableHashTable
ChernClassSymbol = new Type of BasicList
baseName ChernClassSymbol := identity
installMethod(symbol <-, ChernClassSymbol, (c,x) -> chernClassValues#c = x)
value ChernClassSymbol := c -> if chernClassValues#?c then chernClassValues#c else c
expression ChernClassSymbol := c -> new FunctionApplication from {new Subscript from {symbol c,c#0}, c#1}
net ChernClassSymbol := net @@ expression

c = method()
c(ZZ,Symbol) := (n,E) -> value new ChernClassSymbol from {n,E}
c(ZZ,AbstractSheaf) := (n,E) -> part(n,chernClass E)

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
     vrs := apply(bundleNames, bundleRanks, (E,r) -> apply(toList(1 .. r), i -> new ChernClassSymbol from {i,E}));
     dgs := splice apply(bundleRanks, r -> 1 .. r);
     T := (intersectionRing X)[flatten vrs, Degrees => dgs, Global => false, MonomialOrder => apply(bundleRanks, n -> RevLex => n)];
     (A,F,G) := flattenRing T;
     chclasses := apply(vrs, x -> F (1 + sum(x,value)));
     rlns := product chclasses - F promote(chernClass E,T);
     rlns = apply(1 .. sum bundleRanks, i -> part_i rlns);
     B := A/rlns;
     use B;
     DIM := (intersectionRing X).DIM + sum(n, i -> sum(i+1 .. n-1, j -> bundleRanks#i * bundleRanks#j));
     -- point := (basis_(B.DIM) B)_(0,0);			    -- not right, sign could be wrong!
     -- B.point = point;
     FV := abstractVariety(DIM,B);
     scan(n, i -> bundleNames#i <- abstractSheaf(FV,bundleRanks#i, ChernClass => promote(chclasses#i,B)));
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

beginDocumentation()

end

compactMatrixForm = false
loadPackage "Schubert"

Proj(3,{Q,R})
P3 = Proj(3,{Q,R})
A = intersectionRing P3
prune oo
basis A

G24 = Grassmannian(2,4,{Q,R})
C = intersectionRing G24
transpose presentation C

F22 = flagVariety(4,{Q,R},{2,2})
A = intersectionRing F22
transpose presentation A
basis A

F222 = flagVariety(6,{Q,R,S},{2,2,2})
B = intersectionRing F222
transpose presentation B
transpose basis B
(c_1 Q)^3 * (c_1 R)^5 * (c_1 S)^4
integral ((c_1 Q)^3 * (c_1 R)^5 * (c_1 S)^4)

compactMatrixForm = false
loadPackage "Schubert"
X = abstractVariety(3,use (QQ[e1,e2,Degrees=>{1,2}]/(e1^4,e2^2,e1^2*e2)))
E = abstractSheaf(X,2,ChernClass => 1 + e1 + e2)
c_0 E,c_1 E,c_2 E,c_3 E
P1 = Proj(E,{Q,R})					    -- working on this now ...
A = intersectionRing P1
x = c_1 Q
y = c_1 R
x*y

R = QQ[c1,c2,c3,c4,Degrees=>{1,2,3,4},MonomialOrder=>GRevLex=>{1,2,3,4}]
R.DIM = 4
X = abstractVariety(4,R)
F = abstractSheaf(X, 4, ChernClass=>1+sum gens R)
peek F
peek(F ++ F)
segre F
c = 1 + sum gens R
a = logg c
expp a
todd a
