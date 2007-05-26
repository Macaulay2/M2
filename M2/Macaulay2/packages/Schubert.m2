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

export { AbstractSheaf, abstractSheaf, AbstractVariety, abstractVariety, AbstractVarietyMap, adams, Base, BundleRanks, Bundles, CanonicalLineBundle, ch, chern, 
     protect ChernCharacter, protect ChernClass, ChernClassSymbol, chi, ctop, DIM, expp, FlagBundle, flagBundle, FlagBundleStructureMap, integral, protect IntersectionRing,
     intersectionRing, logg, point, PullBack, PushForward, Rank, reciprocal, schur, SectionClass, sectionClass, segre, StructureMap, symm, protect TangentBundle,
     tangentBundle, todd, protect ToddClass, wedge, bundle }

symm = symmetricPower
wedge = exteriorPower

AbstractVariety = new Type of MutableHashTable
AbstractVariety.synonym = "abstract variety"
globalAssignment AbstractVariety
net AbstractVariety := X -> (
     if ReverseDictionary#?X then toString ReverseDictionary#X
     else "a variety")
AbstractVariety.AfterPrint = X -> (
     << endl;				  -- double space
     << concatenate(interpreterDepth:"o") << lineNumber << " : "
     << "an abstract variety of dimension " << dim X << endl;
     )

intersectionRing = method()
intersectionRing AbstractVariety := X -> X.IntersectionRing

FlagBundle = new Type of AbstractVariety
FlagBundle.synonym = "abstract flag bundle"
globalAssignment FlagBundle
net FlagBundle := X -> (
     if ReverseDictionary#?X then toString ReverseDictionary#X
     else "a flag bundle")
FlagBundle.AfterPrint = X -> (
     << endl;				  -- double space
     << concatenate(interpreterDepth:"o") << lineNumber << " : "
     << "a flag bundle with ranks " << X.BundleRanks << endl;
     )

AbstractVarietyMap = new Type of MutableHashTable
AbstractVarietyMap.synonym = "abstract variety map"
FlagBundleStructureMap = new Type of AbstractVarietyMap
FlagBundleStructureMap.synonym = "abstract flag bundle structure map"
AbstractVarietyMap ^* := f -> f.PullBack
AbstractVarietyMap _* := f -> f.PushForward
globalAssignment AbstractVarietyMap
source AbstractVarietyMap := f -> f.source
target AbstractVarietyMap := f -> f.target
dim AbstractVarietyMap := f -> dim source f - dim target f
net AbstractVarietyMap := X -> (
     if ReverseDictionary#?X then toString ReverseDictionary#X
     else "a variety map")
AbstractVarietyMap.AfterPrint = f -> (
     << endl;				  -- double space
     << concatenate(interpreterDepth:"o") << lineNumber << " : "
     << "a map to " << target f << " from " << source f << endl;
     )

sectionClass = method()
sectionClass AbstractVarietyMap := f -> f.SectionClass

AbstractSheaf = new Type of MutableHashTable
AbstractSheaf.synonym = "abstract sheaf"
globalAssignment AbstractSheaf
net AbstractSheaf := X -> (
     if ReverseDictionary#?X then toString ReverseDictionary#X
     else "a sheaf")
AbstractSheaf.AfterPrint = E -> (
     << endl;				  -- double space
     << concatenate(interpreterDepth:"o") << lineNumber << " : "
     << "an abstract sheaf of rank " << rank E << " on " << variety E << endl;
     )

abstractSheaf = method(Options => {
	  ChernClass => null,
	  ChernCharacter => null,
	  Rank => null
	  })
abstractSheaf(AbstractVariety) := opts -> X -> (
     local ch; local rk;
     if opts.ChernCharacter =!= null then (
	  ch = opts.ChernCharacter;
	  rk = part(0,opts.ChernCharacter);
	  try rk = lift(rk,ZZ) else try rk = lift(rk,QQ);
     	  if opts.Rank =!= null and rk != opts.Rank then error "abstractSheaf: expected rank and Chern character to be compatible";
	  )
     else (
     	  if opts.Rank === null then error "abstractSheaf: expected rank or Chern character";
	  rk = opts.Rank;
     	  ch = if opts.ChernClass === null then ch = promote(rk,intersectionRing X) else rk + logg opts.ChernClass;
	  );
     new AbstractSheaf from {
     	  global AbstractVariety => X,
     	  global rank => rk,
	  ChernCharacter => ch,
	  global cache => new CacheTable from {
	       if opts.ChernClass =!= null then ChernClass => opts.ChernClass
	       }
     	  }
     )

abstractSheaf(AbstractVariety,RingElement) := opts -> (X,f) -> abstractSheaf(X, ChernCharacter => f)

abstractVariety = method(Options => { })
abstractVariety(ZZ,Ring) := opts -> (DIM,A) -> (
     if A.?DIM and A.DIM =!= DIM then error "intersection ring corresponds to a variety of a different dimension";
     A.DIM = DIM;
     new AbstractVariety from {
	  global DIM => DIM,
     	  IntersectionRing => A
     	  }
     )

bundle = method()
bundle(ZZ, ZZ, Symbol) := (DIM, rk, nm) -> first bundle(DIM,{rk},{nm})
bundle(ZZ, List, List) := (DIM, rks, nms) -> (
     if not all(nms, s -> instance(s,Symbol)) then error "expected a symbol or symbols";
     vrs := apply(rks, nms, (rk,nm) -> toList apply(1 .. rk, r -> new IndexedVariable from {nm,r}));
     dgs := apply(rks, rk -> toList(1 .. rk));
     A := (intersectionRing point)[flatten vrs, Degrees => flatten dgs, MonomialOrder => apply(dgs, dg -> GRevLex => dg)];
     use A;
     X := abstractVariety(DIM, A);
     toSequence apply(rks, vrs, (rk,e) -> abstractSheaf(X, Rank => rk, ChernClass => 1_A + sum(value \ e))))

tangentBundle = method()
tangentBundle AbstractVariety := X -> (
     if not X.?TangentBundle then error "variety has no tangent bundle";
     X.TangentBundle)
tangentBundle AbstractVarietyMap := f -> (
     if not f.?TangentBundle then error "variety map has no relative tangent bundle";
     f.TangentBundle)

AbstractSheaf QQ := AbstractSheaf ZZ := (F,n) -> (
     if n == 0 then return F;
     X := variety F;
     if X.?CanonicalLineBundle then return F ** X.CanonicalLineBundle^**n;
     error "expected a variety with a canonical line bundle";
     )
AbstractSheaf RingElement := (F,n) -> (
     if n == 0 then return F;
     X := variety F;
     A := intersectionRing X;
     try n = promote(n,A);
     if not instance(n,A) then error "expected an element in the intersection ring of the variety";
     if not isHomogeneous n then error "expected homogeneous element of degree 0 or 1";
     d := first degree n;
     if d == 0 then (
     	  if X.?CanonicalLineBundle 
	  then F ** abstractSheaf(X, Rank => 1, ChernClass => n * chern_1 X.CanonicalLineBundle)
     	  else error "expected a variety with an ample line bundle"
	  )
     else if d == 1 then (
	  F ** abstractSheaf(X, Rank => 1, ChernClass => 1 + n)
	  )
     else error "expected element of degree 0 or 1"
     )     

integral = method()
point = abstractVariety(0,use(QQ[n,Degrees=>{0}]))
point.TangentBundle = abstractSheaf(point,Rank => 0)
integral intersectionRing point := identity
dim AbstractVariety := X -> X.DIM
part(ZZ,QQ) := (n,r) -> if n === 0 then r else 0_QQ

symbol c <- chern = method()
chern AbstractSheaf := (cacheValue ChernClass) (F -> expp F.ChernCharacter)
chern(ZZ, AbstractSheaf) := (p,F) -> part(p,chern F)
chern(ZZ, ZZ, AbstractSheaf) := (p,q,F) -> toList apply(p..q, i -> chern(i,F))
chern(ZZ,Symbol) := (n,E) -> value new ChernClassSymbol from {n,E}

ctop = method()
ctop AbstractSheaf := F -> c_(rank F) F

ch = method()
ch AbstractSheaf := (F) -> F.ChernCharacter
ch(ZZ,AbstractSheaf) := (n,F) -> part_n ch F

chernClassValues = new MutableHashTable
ChernClassSymbol = new Type of BasicList
baseName ChernClassSymbol := identity
installMethod(symbol <-, ChernClassSymbol, (c,x) -> chernClassValues#c = x)
value ChernClassSymbol := c -> if chernClassValues#?c then chernClassValues#c else c
expression ChernClassSymbol := c -> new FunctionApplication from {new Subscript from {symbol c,c#0}, c#1}
net ChernClassSymbol := net @@ expression

OO(AbstractVariety) := X -> (
     A := intersectionRing X;
     abstractSheaf(X, Rank => 1, ChernClass => 1_A, ChernCharacter => 1_A))

AbstractSheaf ^ ZZ := (E,n) -> new AbstractSheaf from {
     global AbstractVariety => E.AbstractVariety,
     ChernCharacter => n * E.ChernCharacter,
     symbol rank => E.rank * n,
     symbol cache => new CacheTable from {
	  if E.cache.?ChernClass then ChernClass => E.cache.ChernClass ^ n
	  }
     }

geometricSeries = (t,n,DIM) -> (			    -- computes (1-t)^n assuming t^(DIM+1) == 0
     ti := 1;
     bin := 1;
     1 + sum for i from 1 to DIM list ( 
	  bin = (1/i) * (n-(i-1)) * bin;
	  ti = ti * t;
	  bin * ti))

AbstractSheaf ^** ZZ := (E,n) -> abstractSheaf(variety E, ChernCharacter => (ch E)^n)
AbstractSheaf ^** QQ := AbstractSheaf ^** RingElement := (E,n) -> (
     if rank E != 1 then error "symbolic power works for invertible sheafs only";
     t := 1 - ch E;
     ti := 1;
     bin := 1;
     abstractSheaf(variety E, Rank => 1, ChernCharacter => geometricSeries(1 - ch E, n, dim variety E)))

rank AbstractSheaf := E -> E.rank
variety AbstractSheaf := E -> E.AbstractVariety

tangentBundle FlagBundle := (stashValue TangentBundle) (FV -> tangentBundle FV.Base + tangentBundle FV.StructureMap)

flagBundle = method()
flagBundle(ZZ,List,List) := (rk,bundleNames,bundleRanks) -> flagBundle(point,rk,bundleNames,bundleRanks)
flagBundle(AbstractVariety,ZZ,List,List) := (X,rk,bundleNames,bundleRanks) -> flagBundle(OO_X^rk,bundleNames,bundleRanks)
flagBundle(AbstractSheaf,List,List) := (E,bundleNames,bundleRanks) -> (
     Ord := GRevLex;
     switch := identity;
     -- bundleNames = reverse bundleNames; bundleRanks = reverse bundleRanks; Ord = RevLex; switch = reverse;
     X := E.AbstractVariety;
     n := #bundleRanks;
     if n =!= #bundleNames then error "name list and rank list should have same length";
     if rank E =!= sum bundleRanks then error "expected rank of bundle to equal sum of bundle ranks";
     bundleNames = apply(bundleNames, n -> if n =!= null and ReverseDictionary#?n then ReverseDictionary#n else n);
     vrs := apply(bundleNames, bundleRanks, (E,r) -> apply(switch toList(1 .. r), i -> new ChernClassSymbol from {i,E}));
     dgs := splice apply(bundleRanks, r -> switch (1 .. r));
     S := intersectionRing X;
     T := S[flatten vrs, Degrees => dgs, Global => false, MonomialOrder => apply(bundleRanks, n -> Ord => n), ConstantCoefficients => false];
     -- (A,F,G) := flattenRing T;
     A := T; F := identity;
     chclasses := apply(vrs, x -> F (1 + sum(x,value)));
     rlns := product chclasses - F promote(chern E,T);
     rlns = sum @@ last \ sort pairs partition(degree,terms(QQ,rlns));
     B := A/rlns;
     -- (C,H,I) := flattenRing B;
     C := B; H := identity;
     use C;
     DIM := dim X + sum(n, i -> sum(i+1 .. n-1, j -> bundleRanks#i * bundleRanks#j));
     FV := C.Variety = new FlagBundle from abstractVariety(DIM,C);
     FV.BundleRanks = bundleRanks;
     FV.Base = X;
     bundles := FV.Bundles = apply(n, i -> (
	       nm := bundleNames#i;
	       bdl := abstractSheaf(FV, Rank => bundleRanks#i, ChernClass => H promote(chclasses#i,B));
	       globalReleaseFunction(nm,value nm);
	       globalAssignFunction(nm,bdl);
	       nm <- bdl));
     if bundleRanks#-1 == 1 then FV.CanonicalLineBundle = last bundles;
     pullback := method();
     pushforward := method();
     pullback ZZ := pullback QQ := r -> pullback promote(r,S);
     pullback S := r -> H promote(F promote(r,T), B);
     sec := product(1 .. n-1, i -> (ctop bundles#i)^(sum(i, j -> rank bundles#j)));
     pushforward C := r -> coefficient(sec,r);
     pullback AbstractSheaf := E -> (
	  if variety E =!= X then "pullback: variety mismatch";
	  abstractSheaf(FV,ChernCharacter => pullback ch E));
     p := new FlagBundleStructureMap from {
	  global target => X,
	  global source => FV,
	  SectionClass => sec,
	  PushForward => pushforward,
	  PullBack => pullback
	  };
     FV.StructureMap = p;
     pushforward AbstractSheaf := E -> (
	  if variety E =!= FV then "pushforward: variety mismatch";
	  abstractSheaf(X,ChernCharacter => pushforward (ch E * todd p)));
     integral C := r -> integral p_* r;
     (FV,p))

tangentBundle FlagBundleStructureMap := (stashValue TangentBundle) (
     p -> (
	  bundles := (source p).Bundles;
	  sum(1 .. #bundles-1, i -> sum(i, j -> Hom(bundles#j,bundles#i)))))

Grassmannian(ZZ,AbstractSheaf,List) := opts -> (k,E,bundleNames) -> flagBundle(E,bundleNames,{rank E-k,k})
Grassmannian(ZZ,ZZ,AbstractVariety,List) := opts -> (k,n,X,bundleNames) -> Grassmannian(k,OO_X^n,bundleNames)
Grassmannian(ZZ,ZZ,List) := opts -> (k,n,bundleNames) -> Grassmannian(k,n,point,bundleNames)

Proj(AbstractSheaf,List) := (E,bundleNames) -> Grassmannian(1,E,bundleNames)
Proj(ZZ,AbstractVariety,List) := (n,X,bundleNames) -> Proj(OO_X^(n+1),bundleNames)
Proj(ZZ,List) := (n,bundleNames) -> Proj(OO_point^(n+1),bundleNames)

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
todd AbstractVariety := X -> todd tangentBundle X
todd AbstractVarietyMap := p -> todd tangentBundle p
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

chi = method()
chi AbstractSheaf := F -> integral(todd variety F * ch F)

segre = method()
segre AbstractSheaf := E -> reciprocal chern dual E
segre(ZZ, AbstractSheaf) := (p,F) -> part(p,segre F)
-- we don't need this one:
-- segre(ZZ, ZZ, AbstractSheaf) := (p,q,F) -> (s := segre F; toList apply(p..q, i -> part(i,s)))

nonnull = x -> select(x, i -> i =!= null)

coerce := (F,G) -> (
     X := variety F;
     Y := variety G;
     if X === Y then return (F,G);
     if X.?StructureMap and target X.StructureMap === Y then return (F, X.StructureMap^* G);
     if Y.?StructureMap and target Y.StructureMap === X then return (Y.StructureMap^* F, G);
     error "expected abstract sheaves on compatible or equal varieties";
     )

AbstractSheaf ++ AbstractSheaf :=
AbstractSheaf + AbstractSheaf := (
     (F,G) -> abstractSheaf nonnull (
	  variety F, Rank => rank F + rank G,
	  ChernCharacter => ch F + ch G,
	  if F.cache.?ChernClass and G.cache.?ChernClass then ChernClass => F.cache.ChernClass * G.cache.ChernClass
	  )) @@ coerce

adams = method()
adams(ZZ,RingElement) := (k,ch) -> (
     d := first degree ch;
     sum(0 .. d, i -> k^i * part_i ch))
adams(ZZ,AbstractSheaf) := (k,E) -> abstractSheaf nonnull (variety E, Rank => rank E, 
     ChernCharacter => adams(k, ch E),
     if E.cache.?ChernClass then ChernClass => adams(k, E.cache.ChernClass)
     )
dual AbstractSheaf := E -> adams(-1,E)

- AbstractSheaf := E -> abstractSheaf(variety E, Rank => - rank E, ChernCharacter => - ch E)
AbstractSheaf - AbstractSheaf := (F,G) -> F + -G

AbstractSheaf ** AbstractSheaf :=
AbstractSheaf * AbstractSheaf := AbstractSheaf => ((F,G) -> abstractSheaf(variety F, Rank => rank F * rank G, ChernCharacter => ch F * ch G)) @@ coerce

Hom(AbstractSheaf, AbstractSheaf) := ((F,G) -> dual F ** G) @@ coerce
End AbstractSheaf := (F) -> Hom(F,F)

det AbstractSheaf := opts -> (F) -> abstractSheaf(variety F, Rank => 1, ChernClass => 1 + part(1,ch F))

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

symmetricPower(ZZ, AbstractSheaf) := symmetricPower(QQ, AbstractSheaf) := symmetricPower(RingElement, AbstractSheaf) := (n,E) -> (
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

loadPackage "Schubert"
compactMatrixForm = false

(P3,p) = Proj(3,{R,Q})
A = intersectionRing P3
c_1 R
c_1 Q
c_2 R
c_2 Q
p_* c_3 Q
p^* 11
transpose presentation A
basis A

(G24,q) = Grassmannian(2,4,{R,Q})
C = intersectionRing G24
transpose presentation C

(F22,r) = flagBundle(4,{R,Q},{2,2})
A = intersectionRing F22
transpose presentation A
basis A

A = QQ[e_1 .. e_4,Degrees=>{1,2,3,4}]
B = A/(e_1^5,e_2^3,e_3^2,e_4^2)
X = abstractVariety(4,B)
E = abstractSheaf(X,Rank => 4,ChernClass => 1 + sum(1 .. 4, i -> e_i))
(F22,p) = flagBundle(E,{R,Q},{2,2})
C = intersectionRing F22
(c_2 R)
(c_2 R)^2
(c_1 Q)^8
p_* (c_1 Q)^8

(F222,p) = flagBundle(6,{P,R,S},{2,2,2})
dim p
B = intersectionRing F222
transpose presentation B
transpose basis B
(c_1 P)^3 * (c_1 R)^5 * (c_1 S)^4
p_* oo

(A,B) = bundle(4,{2,3},{a,b})
chern A
segre B
chern(A**B)
chern(3,symm(3,dual(A)))
segre(2,Hom(wedge(2,A),wedge(2,B)))

(G24,p) = Grassmannian(2,4,{R,Q});
chi symm(n,Q)						    -- doesn't work yet
chi OO_G24(n*c_1 Q)
chi (det Q)^**n
factor oo
p_* (det Q)^**n
assert( (n-2)*(n^3-18*n^2+71*n-6)*(1/12) == chi (det Q)^**n )

E = bundle(3, 3, e)
(P,p) = Proj(E,{W,Q})
C = intersectionRing P
ch Q
netList toList parts ch Q

gens C
degree \ gens C
describe C
c_1 Q
c_1 symbol W
c_1 W
assert( c_1 symbol W == c_1 W )
c_1 W + c_1 Q
assert( c_1 W + c_1 Q == e_1 )
(c_2 W)^2
rank Q
c_1 det Q
promote(e_1,C)
dim p
parts ch Q
assert( value parts ch Q == ch Q )

p_* (c_1 W)^2

-- parameters
(P3,p) = Proj(3,{R,Q});
AP3 = intersectionRing P3;
factor chi OO_P3(n)
todd P3

-- end of Dan's stuff

-- Mike's demo
loadPackage "Schubert"
R = QQ[c1,c2,c3,c4,Degrees=>{1,2,3,4},MonomialOrder=>GRevLex=>{1,2,3,4}]
X = abstractVariety(4,R)
F = abstractSheaf(X, Rank => 4, ChernClass => 1+c1+c2+c3+c4)
segre F
parts oo
chern F
parts oo
ch F
parts oo
netList toList oo
a = logg chern F
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
F = abstractSheaf(X, Rank => 3, ChernClass => 1+c1+c2+c3)
F2 = wedge(2,F)
F3 = wedge(3,F)
symm(1,F)
symm(2,F)
symm(3,F)
G = F**F**F
H = wedge(20,G)
time symm(20,G)
chern H
ch H
chern F
segre F
segre(3,F)
parts segre F
netList toList parts segre F

TEST /// -- segre
  loadPackage "Schubert"
  X = abstractVariety(3, use (QQ[c1,c2,c3,Degrees=>{1,2,3},MonomialOrder=>GRevLex=>{1,2,3}]))
  F = abstractSheaf(X, Rank => 3,ChernClass => 1+c1+c2+c3)
  assert(chern F == 1+c1+c2+c3)
  assert(toString segre F == "c1^3-2*c1*c2+c3+c1^2-c2+c1+1")
  assert(segre(3,F) == c1^3-2*c1*c2+c3)
  netList segre(0,3,F)
///

restart
loadPackage "Schubert"
R = QQ[c4,c3,c2,c1,Degrees=>{4,3,2,1},MonomialOrder=>RevLex,Global=>false]
A = R/truncate(10,ideal vars R)
X = abstractVariety(9, A)
F = abstractSheaf(X, Rank => 4, ChernClass => 1+c1+c2+c3+c4)
F2 = wedge(2,F)
F3 = wedge(3,F)
symm(1,F)
symm(2,F)
symm(3,F)
G = F**F**F
H = wedge(20,G)
time symm(20,G)
chern H
ch H
chern F
segre F
segre(3,F)
parts segre F
netList toList parts segre F

restart
loadPackage "Schubert"

-- # of elliptic cubics on a sextic 4 fold, a schubert-classic example from Rahul:
-- grass(3,6,c):
-- B:=Symm(3,Qc):
-- Proj(X,dual(B),z):
-- A:=Symm(6,Qc)-Symm(3,Qc)&@o(-z):
-- c18:=chern(rank(A),A):
-- lowerstar(X,c18):
-- integral(Gc,%);
-- Ans =  2734099200
(Gc,p) = Grassmannian(3,6,{Sc,Qc});
B = symm_3 Qc;
(X,q) = Proj(dual B,{K,L});
A = symm(6,Qc) - symm(3,Qc) * dual L;
time integral chern A
assert( oo == 2734099200 )

-- conics on a quintic 3-fold, a schubert-classic example from Rahul:
-- grass(3,5,c,all);
-- Proj(X,dual(symm(2,Qc)),z,f);
-- totalspace(X,Gc);
-- B := symm(5,Qc) - symm(3,Qc) * o(-z);
-- integral(chern(11,B));
-- Ans: 609250
(Gc,p) = Grassmannian(3,5,{Sc,Qc});
(X,q) = Proj(dual symm_2 Qc,{K,L});
B = symm(5,Qc) - symm(3,Qc) * dual L;
integral chern B
assert( oo == 609250 )

(Gc,p) = Grassmannian(3,5,{Sc,Qc});
(X,q) = Proj(dual symm_7 Qc,{K,L});
time B = symm(15,Qc) - symm(3,Qc) * dual L
time integral chern B
assert( 99992296084705144978200 == oo)
       
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
F = abstractSheaf(X, Rank => 4, ChernClass => 1+c1+c2+c3+c4)
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
