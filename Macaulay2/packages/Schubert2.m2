newPackage(
	"Schubert2",
	AuxiliaryFiles => true,
    	Version => "0.1",
    	Date => "May, 2007",
	Authors => {
	     {Name => "Daniel R. Grayson", Email => "dan@math.uiuc.edu", HomePage => "http://www.math.uiuc.edu/~dan/"},
	     {Name => "Michael E. Stillman", Email => "mike@math.cornell.edu", HomePage => "http://www.math.cornell.edu/People/Faculty/stillman.html"}
	     },
	HomePage => "http://www.math.uiuc.edu/Macaulay2/",
    	Headline => "computations of characteristic classes for varieties without equations",
    	DebuggingMode => false
    	)

export { AbstractSheaf, abstractSheaf, AbstractVariety, abstractVariety, schubertCycle,
     AbstractVarietyMap, adams, Base, BundleRanks, Bundles,
     CanonicalLineBundle, ch, chern, protect ChernCharacter, protect ChernClass, ChernClassSymbol, chi, ctop, DIM, expp, FlagBundle,
     flagBundle, FlagBundleStructureMap, integral, protect IntersectionRing,
     intersectionRing, logg, point, PullBack, PushForward, Rank, reciprocal, lowerstar,
     schur, SectionClass, sectionClass, segre, StructureMap, Symm, protect TangentBundle, tangentBundle, todd, protect ToddClass, bundle, proj,
     BundleNames, VariableNames, symm, wedge, grass, totalspace, SubBundles, QuotientBundles}

symm = symmetricPower
wedge = exteriorPower
totalspace = source

AbstractVariety = new Type of MutableHashTable
AbstractVariety.synonym = "abstract variety"
globalAssignment AbstractVariety
net AbstractVariety := X -> (
     if ReverseDictionary#?X then toString ReverseDictionary#X
     else "a variety")
AbstractVariety#{Standard,AfterPrint} = X -> (
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
FlagBundle#{Standard,AfterPrint} = X -> (
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
lowerstar = method()
lowerstar(AbstractVarietyMap,Thing) := (f,x) -> f.PushForward x
globalAssignment AbstractVarietyMap
source AbstractVarietyMap := f -> f.source
target AbstractVarietyMap := f -> f.target
dim AbstractVarietyMap := f -> dim source f - dim target f
net AbstractVarietyMap := X -> (
     if ReverseDictionary#?X then toString ReverseDictionary#X
     else "a variety map")
AbstractVarietyMap#{Standard,AfterPrint} = f -> (
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
AbstractSheaf#{Standard,AfterPrint} = E -> (
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

sheaf(ZZ,Array,AbstractVariety) := (rk,classes,X) -> abstractSheaf(X, Rank => rk, ChernClass => 1 + sum toList classes)
sheaf(ZZ,Array) := (rk,classes) -> (
     if #classes === 0 then error "no Chern classes given";
     sheaf(rk,classes,variety ring first classes))

debug Core						    -- needed only for flatmonoid, sigh
netbydegree := f -> (
     if f == 0 then return "0";
     (i,j) := weightRange(first \ degrees (ring f).flatmonoid, f);
     tms := toList apply(i .. j, n -> part_n f);
     tms = select(tms, p -> p != 0);
     if #tms == 1 then return net expression first tms;
     tms = apply(tms, expression);
     tms = apply(tms, e -> if instance(e,Sum) then new Parenthesize from {e} else e);
     net new Sum from tms)

abstractVariety = method(Options => { Type => AbstractVariety })
abstractVariety(ZZ,Ring) := opts -> (DIM,A) -> (
     if A.?DIM then error "ring already in use as an intersection ring";
     A.DIM = DIM;
     net A := netbydegree;
     X := new opts#Type from {
	  global DIM => DIM,
     	  IntersectionRing => A
     	  };
     A.Variety = X)

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

chern = method()
chern AbstractSheaf := (cacheValue ChernClass) (F -> expp F.ChernCharacter)
chern(ZZ, AbstractSheaf) := (p,F) -> part(p,chern F)
chern(ZZ, ZZ, AbstractSheaf) := (p,q,F) -> toList apply(p..q, i -> chern(i,F))
chern(ZZ,Symbol) := (n,E) -> value new ChernClassSymbol from {n,E}

ctop = method()
ctop AbstractSheaf := F -> chern_(rank F) F

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
variety Ring := R -> R.Variety

tangentBundle FlagBundle := (stashValue TangentBundle) (FV -> tangentBundle FV.Base + tangentBundle FV.StructureMap)

assignable = s -> instance(v,Symbol) or null =!= lookup(symbol <-, class v)

offset := 1
flagBundle = method(Options => {
	  BundleNames => null,
	  VariableNames => null
	  })
flagBundle(List) := opts -> (bundleRanks) -> flagBundle(bundleRanks,point,opts)
flagBundle(List,AbstractVariety) := opts -> (bundleRanks,X) -> flagBundle(bundleRanks,OO_X^(sum bundleRanks),opts)
flagBundle AbstractSheaf := opts -> E -> flagBundle({rank E - 1, 1},E,opts)
flagBundle(List,AbstractSheaf) := opts -> (bundleRanks,E) -> (
     h$ := local h$;
     bundleNames := opts.BundleNames;
     varNames := opts.VariableNames;
     if not all(bundleRanks,r -> instance(r,ZZ) and r>=0) then error "expected bundle ranks to be non-negative integers";
     n := #bundleRanks;
     rk := sum bundleRanks;
     if rank E =!= rk then error "expected rank of bundle to equal sum of bundle ranks";
     bundleNames = (
	  if bundleNames === null then null
	  else if instance(bundleNames,Symbol)
     	  then bundleNames = apply(1 .. #bundleRanks, i -> new IndexedVariable from {bundleNames, i})
     	  else if instance(bundleNames,List) then (
	       if #bundleRanks =!= #bundleNames then error "name list and rank list should have same length";
	       apply(bundleNames, n -> (
			 if n =!= null and ReverseDictionary#?n then ReverseDictionary#n else n
			 )))
	  else error "flagBundle BundleNames option: expected a name or list of names");
     verror := () -> error "flagBundle VariableNames option: expected a good name or list of names";
     varNames = (
	  if varNames === null then varNames = h$;
	  if instance(varNames,Symbol)
	  then apply(0 .. #bundleRanks - 1, bundleRanks, (i,r) -> apply(toList(1 .. r), j -> new IndexedVariable from {varNames,(i+offset,j)}))
	  else if instance(varNames,List)
	  then (
	       if #varNames != n then error("expected ", toString n, " bundle names");
	       apply(0 .. #bundleRanks - 1, bundleRanks, (i,r) -> (
		    h := varNames#i;
		    try h = baseName h;
		    if h === null then apply(toList(1 .. r), j -> new IndexedVariable from {h$,(i+offset,j)})
		    else if instance(h,Symbol) then apply(toList(1 .. r), j -> new IndexedVariable from {h,j})
		    else if instance(h,List) then (
			 if #h != r then error("flagBundle: expected variable name sublist of length ",toString r);
			 apply(h, v -> (
				   try v = baseName v;
				   if not assignable v then error "flagBundle: encountered unusable name in variable list";
				   v)))
		    else verror())))
     	  else verror());
     -- done with user-interface preparation and checking
     Ord := GRevLex;
     X := variety E;
     dgs := splice apply(bundleRanks, r -> 1 .. r);
     S := intersectionRing X;
     T := S[flatten varNames, Degrees => dgs, Global => false, MonomialOrder => apply(bundleRanks, n -> Ord => n), ConstantCoefficients => false];
     -- (A,F) := flattenRing T; G := F^-1 ;
     A := T; F := identity;
     chclasses := apply(varNames, x -> F (1 + sum(x,value)));
     rlns := product chclasses - F promote(chern E,T);
     rlns = sum @@ last \ sort pairs partition(degree,terms(QQ,rlns));
     B := A/rlns;
     -- (C,H) := flattenRing B; I := H^-1;
     C := B; H := identity;
     use C;
     DIM := dim X + sum(n, i -> sum(i+1 .. n-1, j -> bundleRanks#i * bundleRanks#j));
     FV := C.Variety = abstractVariety(DIM,C,Type => FlagBundle);
     FV.BundleRanks = bundleRanks;
     FV.Rank = rk;
     FV.Base = X;
     bundles := FV.Bundles = apply(n, i -> (
	       bdl := abstractSheaf(FV, Rank => bundleRanks#i, ChernClass => H promote(chclasses#i,B));
	       if bundleNames =!= null and bundleNames#i =!= null then globalAssign(bundleNames#i,bdl);
	       bdl));
     FV.SubBundles = (() -> ( t := 0; for i from 0 to n-2 list t = t + bundles#i ))();
     FV.QuotientBundles = (() -> ( t := 0; for i from 0 to n-2 list t = t + bundles#(n-1-i) ))();
     FV.CanonicalLineBundle = OO_FV(sum(1 .. #bundles - 1, i -> i * chern(1,bundles#i)));
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
     FV)

tangentBundle FlagBundleStructureMap := (stashValue TangentBundle) (
     p -> (
	  bundles := (source p).Bundles;
	  sum(1 .. #bundles-1, i -> sum(i, j -> Hom(bundles#j,bundles#i)))))

symbol o <- method()
o(RingElement) := h -> OO_(variety ring h) (h)

---- an attempt at total compatibility with Schubert classic, not ready yet!
fixup := h -> if ReverseDictionary#?h then ReverseDictionary#h else try baseName h else error "expected something usable as a name"
-- proj
proj = method()
proj(ZZ,Thing) := (n,h) -> proj(n,fixup h)
proj(ZZ,Symbol) := (n,h) -> (
     hs := toString h;
     nm := pre -> getSymbol(pre | hs);
     X := flagBundle({n,1},BundleNames => {nm "S",nm "Q"}, VariableNames => {global S,{h}});
     globalAssign(nm "A", intersectionRing X);
     globalAssign(nm "P", X);
     X)
-- Proj
proj(Thing,AbstractSheaf,Thing) := (f,A,c) -> proj(fixup f,A,fixup c)
proj(Symbol,AbstractSheaf,Symbol) := (f,A,c) -> (
     cs := toString c;
     nm := pre -> getSymbol(pre | cs);     
     X := flagBundle(A, BundleNames => {nm "S",nm "Q"}, VariableNames => {global S,{c}});
     globalAssign(nm "A", intersectionRing X);
     globalAssign(f, X.StructureMap);
     globalAssign(nm "P", X);
     X)
-- grass
grass = method()
grass(ZZ,ZZ,Thing) := (k,r,c) -> grass(k,r,fixup c)
grass(ZZ,ZZ,Symbol) := (k,r,c) -> (
     cs := toString c;
     nm := pre -> getSymbol(pre | cs);
     X := flagBundle({r-k,k},BundleNames => {nm "S",nm "Q"}, VariableNames => {global T,c});
     globalAssign(nm "A", intersectionRing X);
     globalAssign(nm "G", X);
     X)
-- Grass
grass(Thing,ZZ,AbstractSheaf,Thing) := (f,k,A,c) -> grass(fixup f,k,A,fixup c)
grass(Symbol,ZZ,AbstractSheaf,Symbol) := (f,k,A,c) -> (
     cs := toString c;
     nm := pre -> getSymbol(pre | cs);
     r := rank A;
     X := flagBundle({r-k,k},A,BundleNames => {nm "S",nm "Q"}, VariableNames => {global T,c});
     globalAssign(nm "A", intersectionRing X);
     globalAssign(f, X.StructureMap);
     globalAssign(nm "G", X);
     X)
-- bundle
bundle = method()
bundle(ZZ, ZZ, Thing) := (DIM, rk, nm) -> bundle(DIM,rk, fixup nm)
bundle(ZZ, ZZ, Symbol) := (DIM, rk, nm) -> first bundle(DIM,{rk},{nm})
bundle(ZZ, List, List) := (DIM, rks, nms) -> (
     if not all(nms, s -> instance(s,Symbol)) then error "expected a symbol or symbols";
     vrs := apply(rks, nms, (rk,nm) -> toList apply(1 .. rk, r -> new IndexedVariable from {nm,r}));
     dgs := apply(rks, rk -> toList(1 .. rk));
     A := (intersectionRing point)[flatten vrs, Degrees => flatten dgs, MonomialOrder => apply(dgs, dg -> GRevLex => dg)];
     use A;
     X := abstractVariety(DIM, A);
     toSequence apply(rks, vrs, (rk,e) -> abstractSheaf(X, Rank => rk, ChernClass => 1_A + sum(value \ e))))
----

reciprocal = method()
reciprocal RingElement := (A) -> (
     -- computes 1/A (mod degree >=(d+1))
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

nonnull := x -> select(x, i -> i =!= null)

coerce := (F,G) -> (
     X := variety F;
     Y := variety G;
     if X === Y then return (F,G);
     if X.?StructureMap and target X.StructureMap === Y then return (F, X.StructureMap^* G);
     if Y.?StructureMap and target Y.StructureMap === X then return (Y.StructureMap^* F, G);
     error "expected abstract sheaves on compatible or equal varieties";
     )

AbstractSheaf ++ ZZ := AbstractSheaf + ZZ := (F,n) -> n ++ F
ZZ ++ AbstractSheaf := ZZ + AbstractSheaf := (n,F) -> if n === 0 then F else OO_(Y)^n ++ F

AbstractSheaf ++ AbstractSheaf :=
AbstractSheaf + AbstractSheaf := (
     (F,G) -> abstractSheaf nonnull (
	  variety F, Rank => rank F + rank G,
	  ChernCharacter => ch F + ch G,
	  if F.cache.?ChernClass and G.cache.?ChernClass then 
	    ChernClass => F.cache.ChernClass * G.cache.ChernClass
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
     -- wedge is an array 0..n of the chern characters of the exerior 
     -- powers of E.  The last one is what we want.
     if 2*n > rank E then return det(E) ** dual exteriorPower(rank E - n, E);
     wedge := computeWedges(n,ch E);
     abstractSheaf(variety E, ChernCharacter => wedge#n)
     )

Symm = method()
Symm(ZZ,AbstractSheaf) :=
Symm(RingElement,AbstractSheaf) := (n,F) -> (
     X := variety F;
     A := intersectionRing X;
     try n = promote(n,A);
     if not instance(n,A) then error "expected an element in the intersection ring of the variety";
     if not isHomogeneous n or degree n =!= {0} then error "expected homogeneous element of degree 0";
     -- This uses Grothendieck-Riemann-Roch, together with the fact that
     -- f_!(OO_PF(n)) = f_*(symm(n,F)), since the higher direct images are 0.
     h := local h;
     PF := flagBundle(F, VariableNames => h);
     f := PF.StructureMap;
     abstractSheaf(X, f_*(ch OO_PF(n) * todd f))
     )

symmetricPower(ZZ, AbstractSheaf) := 
symmetricPower(QQ, AbstractSheaf) := 
symmetricPower(RingElement, AbstractSheaf) := (n,E) -> (
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

schubertCycle = method()
FlagBundle _ Sequence := schubertCycle
FlagBundle _ List := schubertCycle
giambelli =  (r,E,b) -> (
     p := matrix for i from 0 to r-1 list for j from 0 to r-1 list chern(b#i-i+j,E); -- Giambelli's formula, also called Jacobi-Trudi
     if debugLevel > 15 then stderr << "giambelli : " << p << endl;
     det p
     )
listtoseq = (r,b) -> toSequence apply(#b, i -> r + i - b#i)
seqtolist = (r,b) ->            apply(#b, i -> r + i - b#i)
dualpart  = (r,b) -> splice for i from 0 to #b list ((if i === #b then r else b#(-i-1)) - (if i === 0 then 0 else b#-i)) : #b - i

schubertCycle(FlagBundle,Sequence) := (X,a) -> (
     if #X.BundleRanks != 2 then error "expected a Grassmannian";
     n := X.Rank;
     E := last X.Bundles;
     r := rank E;
     r' := n-r;
     if r != #a then error("expected a sequence of length ", toString r);
     for i from 0 to r-1 do (
	  ai := a#i;
	  if not instance(ai,ZZ) or ai < 0 then error "expected a sequence of non-negative integers";
	  if i>0 and not (a#(i-1) < a#i) then error "expected a strictly increasing sequence of integers";
	  if not (ai < n) then error("expected a sequence of integers less than ",toString n);
	  );
     giambelli(r',E,dualpart(r',seqtolist(r',a))))
schubertCycle(FlagBundle,List) := (X,b) -> (
     -- see page 271 of Fulton's Intersection Theory for this notation
     if #X.BundleRanks != 2 then error "expected a Grassmannian";
     E := last X.Bundles;
     r := rank E;
     n := X.Rank;
     r' := n-r;
     if r != #b then error("expected a list of length ", toString r);
     for i from 0 to r-1 do (
	  bi := b#i;
	  if not instance(bi,ZZ) or bi < 0 then error "expected a list of non-negative integers";
	  if i>0 and not (b#(i-1) >= b#i) then error "expected a decreasing list of integers";
	  if not (bi <= r') then error("expected a list of integers bounded by ",toString(n-r));
	  );
     giambelli(r',E,dualpart(r',b)))

beginDocumentation()

-- various demos moved to Schubert2/.
