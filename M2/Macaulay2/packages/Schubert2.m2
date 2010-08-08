-- -*- coding: utf-8 -*-
newPackage(
	"Schubert2",
	AuxiliaryFiles => true,
    	Version => "0.3",
    	Date => "December, 2009",
	Authors => {
	     {Name => "Daniel R. Grayson", Email => "dan@math.uiuc.edu", HomePage => "http://www.math.uiuc.edu/~dan/"},
	     {Name => "Michael E. Stillman", Email => "mike@math.cornell.edu", HomePage => "http://www.math.cornell.edu/People/Faculty/stillman.html"},
	     {Name => "Stein A. Strømme", Email => "stromme@math.uib.no", HomePage => "http://stromme.uib.no/home/" },
	     {Name => "David Eisenbud", Email => "de@msri.org", HomePage => "http://www.msri.org/~de/"}
	     },
	HomePage => "http://www.math.uiuc.edu/Macaulay2/",
    	Headline => "computations of characteristic classes for varieties without equations"
    	)

export { "AbstractSheaf", "abstractSheaf", "AbstractVariety", "abstractVariety", "schubertCycle", "ReturnType",
     "AbstractVarietyMap", "adams", "Base", "BundleRanks", "Bundles", "VarietyDimension", "Bundle",
     "TautologicalLineBundle", "ch", "chern", "ChernCharacter", "ChernClass", "ChernClassVariable", "chi", "ctop", "FlagBundle",
     "flagBundle", "projectiveBundle", "projectiveBundle'", "projectiveSpace", "projectiveSpace'", "PP", "PP'", "integral", "IntersectionRing",
     "intersectionRing", "PullBack", "PushForward", "Rank", "ChernClassVariableTable",
     "schur", "SectionClass", "sectionClass", "segre", "StructureMap", "TangentBundle", "tangentBundle", "cotangentBundle", "todd",
     "sectionZeroLocus", "degeneracyLocus", "degeneracyLocus2", "kernelBundle",
     "VariableNames", "VariableName", "SubBundles", "QuotientBundles", "point", "base"}

-- not exported, for now: "logg", "expp", "reciprocal", "ToddClass"

protect ChernCharacter
protect ChernClass
protect IntersectionRing
protect TangentBundle
protect ToddClass

hasAttribute = value Core#"private dictionary"#"hasAttribute"
getAttribute = value Core#"private dictionary"#"getAttribute"
ReverseDictionary = value Core#"private dictionary"#"ReverseDictionary"
indexSymbols = value Core#"private dictionary"#"indexSymbols"

AbstractVariety = new Type of MutableHashTable
AbstractVariety.synonym = "abstract variety"
globalAssignment AbstractVariety
toString AbstractVariety := net AbstractVariety := X -> (
     if hasAttribute(X,ReverseDictionary) then toString getAttribute(X,ReverseDictionary)
     else "a variety")
AbstractVariety#{Standard,AfterPrint} = X -> (
     << endl;				  -- double space
     << concatenate(interpreterDepth:"o") << lineNumber << " : "
     << "an abstract variety of dimension " << X.dim << endl;
     )

intersectionRing = method(TypicalValue => Ring)
intersectionRing AbstractVariety := X -> X.IntersectionRing

FlagBundle = new Type of AbstractVariety
FlagBundle.synonym = "abstract flag bundle"
net FlagBundle := toString FlagBundle := X -> (
     if hasAttribute(X,ReverseDictionary) then toString getAttribute(X,ReverseDictionary)
     else "a flag bundle")
FlagBundle#{Standard,AfterPrint} = X -> (
     << endl;				  -- double space
     << concatenate(interpreterDepth:"o") << lineNumber << " : "
     << "a flag bundle with ranks " << X.BundleRanks << endl;
     )

AbstractVarietyMap = new Type of MutableHashTable
AbstractVarietyMap.synonym = "abstract variety map"
AbstractVarietyMap ^* := Function => f -> f.PullBack
AbstractVarietyMap _* := Function => f -> f.PushForward
globalAssignment AbstractVarietyMap
source AbstractVarietyMap := AbstractVariety => f -> f.source
target AbstractVarietyMap := AbstractVariety => f -> f.target
dim AbstractVarietyMap := f -> dim source f - dim target f
toString AbstractVarietyMap := net AbstractVarietyMap := X -> (
     if hasAttribute(X,ReverseDictionary) then toString getAttribute(X,ReverseDictionary)
     else "a variety map")
AbstractVarietyMap#{Standard,AfterPrint} = f -> (
     << endl;				  -- double space
     << concatenate(interpreterDepth:"o") << lineNumber << " : "
     << "a map to " << target f << " from " << source f << endl;
     )
AbstractVarietyMap * AbstractVarietyMap := AbstractVarietyMap => (f,g) -> new AbstractVarietyMap from {
     symbol source => source g,
     symbol target => target f,
     PullBack => g.PullBack @@ f.PullBack,
     PushForward => f.PushForward @@ g.PushForward,	    -- may not be efficient
     if g.?SectionClass and f.?SectionClass then SectionClass => g.SectionClass * g.PullBack f.SectionClass
     }

map(FlagBundle,AbstractVarietyMap,List) := AbstractVarietyMap => x -> notImplemented()
map(FlagBundle,AbstractVariety,List) := AbstractVarietyMap => x -> notImplemented()
AbstractVariety#id = (X) -> new AbstractVarietyMap from {
     symbol source => X,
     symbol target => X,
     Pullback => id_(intersectionRing X),
     PushForward => identity,
     SectionClass => 1_(intersectionRing X)
     }
AbstractVariety / AbstractVariety := AbstractVarietyMap => (X,S) -> (
     maps := while X =!= S and X.?StructureMap list (f := X.StructureMap; X = target f; f);
     if #maps == 0 then id_X
     else fold(maps,(f,g) -> g * f))

sectionClass = method(TypicalValue => RingElement)
sectionClass AbstractVarietyMap := f -> f.SectionClass

AbstractSheaf = new Type of HashTable
AbstractSheaf.synonym = "abstract sheaf"
baseName AbstractSheaf := F -> if F.cache.?Name then F.cache.Name else error "unnamed abstract sheaf"
globalAssignment AbstractSheaf
net AbstractSheaf := toString AbstractSheaf := X -> (
     if hasAttribute(X,ReverseDictionary) then toString getAttribute(X,ReverseDictionary)
     else "a sheaf")
AbstractSheaf#{Standard,AfterPrint} = E -> (
     << endl;				  -- double space
     << concatenate(interpreterDepth:"o") << lineNumber << " : "
     << "an abstract sheaf of rank " << rank E << " on " << variety E << endl;
     )

abstractSheaf = method(
     TypicalValue => AbstractSheaf,
     Options => {
	  Name => null,
	  ChernClass => null,
	  ChernCharacter => null,
	  Rank => null,
	  })
abstractSheaf AbstractVariety := opts -> X -> (
     local ch; local rk;
     A := intersectionRing X;
     if opts.ChernCharacter =!= null then (
	  ch = opts.ChernCharacter;
	  ch = promote(ch,A);
	  rk = part(0,opts.ChernCharacter);
	  )
     else if opts.Rank =!= null then (
	  ch = rk = promote(opts.Rank,A);
	  if opts.ChernClass =!= null then ch = ch + logg promote(opts.ChernClass,A);
	  )
     else error "expected Rank or ChernCharacter option";
     try rk = lift(rk,ZZ) else try rk = lift(rk,QQ);
     new AbstractSheaf from {
     	  global AbstractVariety => X,
	  ChernCharacter => ch,
	  global cache => new CacheTable from {
	       if opts.Name =!= null then Name => opts.Name,
     	       global rank => rk,
	       if opts.ChernClass =!= null then ChernClass => promote(opts.ChernClass,A)
	       }
     	  }
     )
abstractSheaf(AbstractVariety,ZZ) := 
abstractSheaf(AbstractVariety,QQ) := 
abstractSheaf(AbstractVariety,RingElement) := opts -> (X,f) -> abstractSheaf(X, ChernCharacter => f)

bydegree := net -> f -> (
     if f == 0 then return "0";
     (i,j) := weightRange(first \ degrees (ring f).FlatMonoid, f);
     tms := toList apply(i .. j, n -> part_n f);
     tms = select(tms, p -> p != 0);
     if #tms == 1 then return net expression first tms;
     tms = apply(tms, expression);
     tms = apply(tms, e -> if instance(e,Sum) then new Parenthesize from {e} else e);
     net new Sum from tms)

abstractVariety = method(TypicalValue => AbstractVariety, Options => { ReturnType => AbstractVariety })
abstractVariety(ZZ,Ring) := opts -> (d,A) -> (
     if A.?VarietyDimension then error "ring already in use as an intersection ring";
     if ultimate(coefficientRing,A) =!= QQ then error "expected a QQ-algebra";
     if degreeLength A != 1 then error "expected a ring with degree length 1";
     A.VarietyDimension = d;
     net A := bydegree net;
     toString A := bydegree toString;
     if not ancestor(AbstractVariety,opts#ReturnType) then error "expected value of ReturnType option to be a type of AbstractVariety";
     integral A := (
	  if d === 0
	  then x -> part(d,x)
	  else x -> (hold integral) part(d,x)
	  );	  
     A.Variety = new opts#ReturnType from { global dim => d, IntersectionRing => A })

tangentBundle = method(TypicalValue => AbstractSheaf)
cotangentBundle = method(TypicalValue => AbstractSheaf)
tangentBundle AbstractVariety := X -> (
     if not X.?TangentBundle then error "variety has no tangent bundle";
     X.TangentBundle)
cotangentBundle AbstractVariety := X -> dual tangentBundle X
tangentBundle AbstractVarietyMap := f -> (
     if not f.?TangentBundle 
     then tangentBundle source f - f^* tangentBundle target f
     else f.TangentBundle)
cotangentBundle AbstractVarietyMap := f -> dual tangentBundle f

euler AbstractVariety := X -> integral ctop tangentBundle X

AbstractSheaf QQ := AbstractSheaf ZZ := AbstractSheaf => (F,n) -> (
     if n == 0 then return F;
     X := variety F;
     if not X.?TautologicalLineBundle then error "expected a variety with a tautological line bundle";
     L := (
	  if n == 1
	  then X.TautologicalLineBundle
	  else abstractSheaf(X, Rank => 1, ChernClass => 1 + n * chern_1 X.TautologicalLineBundle)
	  );
     F ** L)
AbstractSheaf RingElement := AbstractSheaf => (F,n) -> (
     if n == 0 then return F;
     X := variety F;
     A := intersectionRing X;
     try n = promote(n,A);
     if not instance(n,A) then error "expected an element in the intersection ring of the variety";
     if not isHomogeneous n then error "expected homogeneous element of degree 0 or 1";
     d := first degree n;
     if d == 0 then (
     	  if X.?TautologicalLineBundle 
	  then F ** abstractSheaf(X, Rank => 1, ChernClass => n * chern_1 X.TautologicalLineBundle)
     	  else error "expected a variety with an ample line bundle"
	  )
     else if d == 1 then (
	  F ** abstractSheaf(X, Rank => 1, ChernClass => 1 + n)
	  )
     else error "expected element of degree 0 or 1"
     )     

integral = method(TypicalValue => RingElement)

protect Bundle

base = method(Dispatch => Thing, TypicalValue => AbstractVariety)
base Thing := s -> base (1:s)
base Sequence := args -> (
     -- up to one integer, specifying the dimension d of the base
     -- some symbols or indexed variables, to be used as parameter variables of degree 0
     -- some options Bundle => (B,n,b), where B is a symbol or an indexed variable, b is a symbol, and n is an integer, 
     --    specifying that we should provide a bundle named B of rank n whose Chern classes are b_1,...,b_n,
     --    but if n > d then it goes b_1,...,b_d
     degs := vrs := ();
     bdls := {};
     newvr  := (x,d) -> (vrs = (vrs,x);degs = (degs,d));
     newbdl := x -> bdls = append(bdls,x);
     d := null;
     oops := x -> error ("base: unrecognizable argument ",toString x);
     goodvar := x -> try baseName x else error ("unusable as variable: ",toString x);
     goodsym := x -> (
	  s := try baseName x else x;
	  if not instance(s,Symbol) then error ("unusable as subscripted symbol: ",toString x);
	  s);
     scan(args, x -> (
	       if instance(x,Symbol) or instance(x,IndexedVariable) then newvr(x,0)
	       else if instance(x,RingElement) then newvr(baseName x,0)
	       else if instance(x,Option) and #x==2 and x#0 === Bundle and instance(x#1,Sequence) and #x#1== 3 then (
		    (B,n,b) := x#1;
		    if not instance(n,ZZ) then oops x;
     		    if d === null then d = 0;
		    nd := min(n,d);
		    if instance(b,VisibleList) then (
			 if length b != nd then error("expected ",toString nd," variables for the Chern classes of ",toString B);
			 b = apply(toSequence b, goodvar);
			 )
		    else if instance(b,String) then (
			 b = apply(1..nd,i->getSymbol(b|toString i));
			 )
		    else (
		    	 b = goodsym b;
			 b = apply(1..nd,i->b_i);
			 );
		    vrs = (vrs,b);
		    degs = (degs,1..nd);
		    B = goodvar B;
		    newbdl (B,n,b);
		    )
	       else if instance(x,ZZ) then (
		    if #bdls > 0 then error "integer argument (the dimension) should be first";
		    if d =!= null then error "more than one integer argument encountered (as the dimension)";
		    d = x)
	       else oops x));
     if d === null then d = 0;
     vrs = deepSplice vrs;
     degs = toList deepSplice degs;
     A := QQ[vrs,Degrees => degs, DegreeRank => 1];
     X := abstractVariety(d,A);
     X.TangentBundle = abstractSheaf(X,Rank => d);          -- it's the base; user can replace it
     X.TautologicalLineBundle = abstractSheaf(X,Rank => 1); -- it's the base; user can replace it
     integral intersectionRing X := (
	  if d === 0
	  then x -> part(d,x)
	  else x -> (hold integral) part(d,x)
	  );
     X#"bundles" = apply(bdls,(B,n,b) -> (
	       globalReleaseFunction(B,value B);
	       B <- abstractSheaf(X, Name => B, Rank => n, ChernClass => 1_A + sum(1 .. min(n,d), i -> A_(b#(i-1))));
	       globalAssignFunction(B,value B);
	       (B,value B)));
     X.args = args;
     X)
point = base()
integral intersectionRing point := r -> if liftable(r,ZZ) then lift(r,ZZ) else lift(r,QQ)

dim AbstractVariety := X -> X.dim

chern = method(TypicalValue => RingElement)
chern AbstractSheaf := (cacheValue ChernClass) (F -> expp F.ChernCharacter)
chern(ZZ, AbstractSheaf) := (p,F) -> part(p,chern F)
chern(ZZ, ZZ, AbstractSheaf) := List => (p,q,F) -> toList apply(p..q, i -> chern(i,F))

ctop = method(TypicalValue => RingElement)
ctop AbstractSheaf := F -> chern_(rank F) F

ch = method(TypicalValue => RingElement)
ch AbstractSheaf := (F) -> F.ChernCharacter
ch(ZZ,AbstractSheaf) := (n,F) -> part_n ch F

ChernClassVariableTable = new Type of MutableHashTable
net ChernClassVariableTable := Etable -> net Etable # symbol$
baseName ChernClassVariableTable := Etable -> Etable # symbol$
ChernClassVariable = new Type of BasicList
ChernClassVariable.synonym = "Chern class variable"
getCCVTable = E -> (
     Etable := value E;
     if not instance(Etable,ChernClassVariableTable) then (
	  if E =!= Etable then stderr << "--warning: clearing value of symbol " << E << " to allow access to Chern class variables based on it" << endl;
	  E <- Etable = new ChernClassVariableTable;
	  Etable#symbol$ = E;
	  );
     Etable)
chern(ZZ,Symbol) := ChernClassVariable => (n,E) -> ( getCCVTable E; new ChernClassVariable from {n,E} )
chern(ZZ,ChernClassVariableTable) := (n,E) -> if E#?n then E#n else new ChernClassVariable from {n,E#symbol$}
Ring _ ChernClassVariable := (R,s) -> R#indexSymbols#s
baseName ChernClassVariable := identity
installMethod(symbol <-, ChernClassVariable, (c,x) -> (getCCVTable c#1)#(c#0) = x)
value ChernClassVariable := c -> ( n := c#0; E := c#1; Etable := value E; if instance(Etable,ChernClassVariableTable) then Etable#n else c)
expression ChernClassVariable := c -> new FunctionApplication from {new Subscript from {symbol c,c#0}, c#1}
net ChernClassVariable := net @@ expression
toString ChernClassVariable := toString @@ expression
ChernClassVariable .. ChernClassVariable := (a,b) -> (
     if a#1 =!= b#1 then error "expected Chern class variables based on the same symbol";
     apply(a#0 .. b#0, i -> new ChernClassVariable from {i,a#1} ))

installMethod(symbol _, OO, RingElement, AbstractSheaf => (OO,D) -> (
	  if D != 0 and degree D != {1} then error "expected a cycle class of degree 1 (a divisor class)";
	  1 - OO_(variety D)(-D)))
installMethod(symbol _, OO, AbstractVariety, AbstractSheaf => 
     (OO,X) -> (
	  A := intersectionRing X;
	  abstractSheaf(X, Rank => 1, ChernCharacter => 1_A, ChernClass => 1_A))
     )

AbstractSheaf * RingElement := 
AbstractSheaf * QQ := (E,n) -> n*E
RingElement * AbstractSheaf := (n,E) -> (
     if degree n =!= {0} then error "expected a ring element of degree 0";
     abstractSheaf(variety E, ChernCharacter => n * ch E))
QQ * AbstractSheaf := AbstractSheaf => (n,E) -> abstractSheaf(variety E, ChernCharacter => n * ch E)

ZZ * AbstractSheaf := AbstractSheaf => (n,E) -> E^n
AbstractSheaf * ZZ := 
AbstractSheaf ^ ZZ := AbstractSheaf => (E,n) -> new AbstractSheaf from {
     global AbstractVariety => E.AbstractVariety,
     ChernCharacter => n * E.ChernCharacter,
     symbol cache => new CacheTable from {
     	  global rank => E.cache.rank * n,
	  if E.cache.?ChernClass and n >= 0 then ChernClass => E.cache.ChernClass ^ n
	  }
     }

geometricSeries = (t,n,dim) -> (			    -- computes (1+t)^n assuming t^(dim+1) == 0
     ti := 1;
     bin := 1;
     r := 1;
     for i from 1 to dim do (
	  bin = (1/i) * (n-(i-1)) * bin;
	  ti = ti * t;
	  r = r + bin * ti);
     r)

AbstractSheaf ^** ZZ := AbstractSheaf => (E,n) -> (
     if n < 0 then (
	  if rank E =!= 1 then error "negative tensor power of sheaf of rank not equal to 1 requested";
	  E = dual E;
	  n = - n;
	  );
     abstractSheaf(variety E, ChernCharacter => part(0,dim variety E,(ch E)^n)))

AbstractSheaf ^** QQ := AbstractSheaf ^** RingElement := AbstractSheaf => (E,n) -> (
     if rank E != 1 then error "non-integer tensor power of sheaf of rank not equal to 1 requested";
     abstractSheaf(variety E, Rank => 1, ChernCharacter => geometricSeries(ch E - 1, n, dim variety E)))

rank AbstractSheaf := RingElement => E -> E.cache.rank
variety AbstractSheaf := AbstractVariety => E -> E.AbstractVariety
variety Ring := AbstractVariety => R -> R.Variety
variety RingElement := AbstractVariety => r -> variety ring r

tangentBundle FlagBundle := (stashValue TangentBundle) (FV -> tangentBundle FV.Base + tangentBundle FV.StructureMap)

assignable = s -> instance(v,Symbol) or null =!= lookup(symbol <-, class v)

offset := 1
flagBundle = method(Options => { VariableNames => null }, TypicalValue => FlagBundle)
flagBundle(List) := opts -> (bundleRanks) -> flagBundle(bundleRanks,point,opts)
flagBundle(List,AbstractVariety) := opts -> (bundleRanks,X) -> flagBundle(bundleRanks,OO_X^(sum bundleRanks),opts)
flagBundle(List,AbstractSheaf) := opts -> (bundleRanks,E) -> (
     h$ := global H;
     varNames := opts.VariableNames;
     bundleRanks = splice bundleRanks;
     if not all(bundleRanks,r -> instance(r,ZZ) and r>=0) then error "expected bundle ranks to be non-negative integers";
     n := #bundleRanks;
     rk := sum bundleRanks;
     hft := {1};
     HR := degreesRing hft;
     T := HR_0;
     vn := dualpart(0,rsort bundleRanks);
     hilbertSeriesHint := new Divide from {
	  promote( product for i from 0 to n-1 list ( k := sum take(bundleRanks,i); product for j from k+1 to k+bundleRanks#i list 1 - T^j ), HR),
	  new Product from reverse for i from 1 to #vn list new Power from {1 - T^i, vn#(i-1)}
	  };
     if rank E != rk then error "expected rank of bundle to equal sum of bundle ranks";
     if part(rk+1,infinity,chern E) != 0 then error "expected an effective bundle (vanishing higher Chern classes)";
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
		    else if instance(h,String) then apply(toList(1 .. r), j -> getSymbol(h|toString j))
		    else if instance(h,List) then (
			 h = splice h;
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
     U := S(monoid [flatten varNames, Degrees => dgs, MonomialOrder => apply(bundleRanks, n -> Ord => n), Join => false, Heft => hft, DegreeRank => 1]);
     -- (A,F) := flattenRing U; G := F^-1 ;
     A := U; F := identity;
     chclasses := apply(varNames, x -> F (1_U + sum(x,v -> U_v)));
     rlns := product chclasses - F promote(chern E,U);
     rlns = sum @@ last \ sort pairs partition(degree,terms(QQ,rlns));
     rlns = ideal matrix(U,{rlns});
     if heft S =!= null and degreesRing S === HR then gb(rlns, Hilbert => numerator hilbertSeriesHint * numerator hilbertSeries S);
     B := A/rlns;
     -- (C,H) := flattenRing B; I := H^-1;
     C := B; H := identity;
     -- use C;
     C#"hilbert Function hint" = hilbertSeriesHint;
     d := dim X + sum(n, i -> sum(i+1 .. n-1, j -> bundleRanks#i * bundleRanks#j));
     FV := C.Variety = abstractVariety(d,C,ReturnType => FlagBundle);
     FV.BundleRanks = bundleRanks;
     FV.Rank = rk;
     FV.Base = X;
     bundles := FV.Bundles = apply(0 .. n-1, i -> (
	       bdl := abstractSheaf(FV, Rank => bundleRanks#i, ChernClass => H promote(chclasses#i,B));
	       bdl));
     FV.SubBundles = (() -> ( t := OO_FV^0; for i from 0 to n list if i == 0 then t else t = t + bundles#(i-1)))();
     FV.QuotientBundles = (() -> ( t := OO_FV^0; for i from 0 to n list if i == 0 then t else t = t + bundles#(n-i)))();
     FV.TautologicalLineBundle = OO_FV(sum(1 .. #bundles - 1, i -> i * chern(1,bundles#i)));
     pullback := method();
     pushforward := method();
     pullback ZZ := pullback QQ := r -> promote(r,C);
     pullback S := r -> H promote(F promote(r,U), B);
     sec := if n === 0 then 1_C else product(1 .. n-1, i -> (ctop bundles#i)^(sum(i, j -> rank bundles#j)));
     pushforward C := r -> coefficient(sec,r);
     pullback AbstractSheaf := E -> (
	  if variety E =!= X then "pullback: variety mismatch";
	  abstractSheaf(FV,Rank => rank E, ChernClass => pullback chern E));
     p := new AbstractVarietyMap from {
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
     FV.StructureMap.TangentBundle = (
	  if #bundles > 0
	  then sum(1 .. #bundles-1, i -> sum(i, j -> Hom(bundles#j,bundles#i)))
	  else OO_FV^0);
     if X.?TangentBundle then FV.TangentBundle = FV.StructureMap.TangentBundle + FV.StructureMap^* X.TangentBundle;
     use FV;
     FV)

use AbstractVariety := AbstractVariety => X -> (
     use intersectionRing X;
     if X#?"bundles" then scan(X#"bundles",(sym,shf) -> sym <- shf);
     X)

installMethod(symbol SPACE, OO, RingElement, AbstractSheaf => (OO,h) -> OO_(variety ring h) (h))

projectiveBundle = method(Options => { VariableNames => null }, TypicalValue => FlagBundle)
projectiveBundle ZZ := opts -> n -> flagBundle({n,1},opts)
projectiveBundle(ZZ,AbstractVariety) := opts -> (n,X) -> flagBundle({n,1},X,opts)
projectiveBundle AbstractSheaf := opts -> E -> flagBundle({rank E - 1, 1},E,opts)

projectiveBundle' = method(Options => { VariableNames => null }, TypicalValue => FlagBundle)
projectiveBundle' ZZ := opts -> n -> flagBundle({1,n},opts)
projectiveBundle'(ZZ,AbstractVariety) := opts -> (n,X) -> flagBundle({1,n},X,opts)
projectiveBundle' AbstractSheaf := opts -> E -> flagBundle({1, rank E - 1},E,opts)

projectiveSpace = method(Options => { VariableName => global h }, TypicalValue => FlagBundle)
projectiveSpace ZZ := opts -> n -> flagBundle({n,1},VariableNames => {,{opts.VariableName}})
projectiveSpace(ZZ,AbstractVariety) := opts -> (n,X) -> flagBundle({n,1},X,VariableNames => {,{opts.VariableName}})

projectiveSpace' = method(Options => { VariableName => global h }, TypicalValue => FlagBundle)
projectiveSpace' ZZ := opts -> n -> flagBundle({1,n},VariableNames => {{opts.VariableName},})
projectiveSpace'(ZZ,AbstractVariety) := opts -> (n,X) -> flagBundle({1,n},X,VariableNames => {{opts.VariableName},})

PP  = new ScriptedFunctor from { superscript => i -> projectiveSpace i }
PP' = new ScriptedFunctor from { superscript => i -> projectiveSpace' i }

reciprocal = method(TypicalValue => RingElement)
reciprocal RingElement := (A) -> (
     -- computes 1/A (mod degree >=(d+1))
     -- ASSUMPTION: part(0,A) == 1.
     d := (ring A).VarietyDimension;
     a := for i from 0 to d list part_i(A);
     recip := new MutableList from splice{d+1:0};
     recip#0 = 1_(ring A);
     for n from 1 to d do
       recip#n = - sum(1..n, i -> a#i * recip#(n-i));
     sum toList recip
     )

logg = method(TypicalValue => RingElement)
logg QQ := logg ZZ := (n) -> 0
logg RingElement := (C) -> (
     -- C is the total chern class in an intersection ring A
     -- The chern character of C is returned.
     A := ring C;
     d := A.VarietyDimension;
     p := new MutableList from splice{d+1:0}; -- p#i is (-1)^i * (i-th power sum of chern roots)
     e := for i from 0 to d list part(i,C); -- elem symm functions in the chern roots
     for n from 1 to d do
         p#n = -n*e#n - sum for j from 1 to n-1 list e#j * p#(n-j);
     promote(sum for i from 1 to d list 1/i! * (-1)^i * p#i, A))

expp = method(TypicalValue => RingElement)
expp QQ := expp ZZ := (n) -> 1
expp RingElement := (y) -> (
     -- y is the chern character
     -- the total chern class of y is returned
     A := ring y;
     d := A.VarietyDimension;
     p := for i from 0 to d list (-1)^i * i! * part(i,y);
     e := new MutableList from splice{d+1:0};
     e#0 = 1_A;
     for n from 1 to d do
	  e#n = - 1/n * sum for j from 1 to n list p#j * e#(n-j);
     sum toList e
     )

todd = method(TypicalValue => RingElement)
todd AbstractSheaf := E -> todd' ch E
todd AbstractVariety := X -> todd tangentBundle X
todd AbstractVarietyMap := p -> todd tangentBundle p
todd' = (r) -> (
     -- r is the chern character
     -- the (total) todd class is returned
     A := ring r;
     if not A.?VarietyDimension then error "expected a ring with its variety dimension set";
     if r == 0 then return 1_A;
     d := A.VarietyDimension;
     -- step 1: find the first part of the Taylor series for t/(1-exp(-t))
     denom := for i from 0 to d list (-1)^i /(i+1)!;
     invdenom := new MutableList from splice{d+1:0};
     invdenom#0 = 1;
     for n from 1 to d do 
       invdenom#n = - sum for i from 1 to n list denom#i * invdenom#(n-i);
     -- step 2.  logg.  This is more complicated than desired.
     R := QQ (monoid[t]);
     R.VarietyDimension = d;
     td := logg sum for i from 0 to d list invdenom#i * R_0^i;
     td = for i from 0 to d list coefficient(R_0^i,td);
     -- step 3.  exp
     expp sum for i from 0 to d list i! * td#i * part(i,r))

chi = method(TypicalValue => RingElement)
chi AbstractSheaf := F -> integral(todd variety F * ch F)

segre = method(TypicalValue => RingElement)
segre AbstractSheaf := E -> reciprocal chern dual E
segre(ZZ, AbstractSheaf) := (p,F) -> part(p,segre F)
-- we don't need this one:
-- segre(ZZ, ZZ, AbstractSheaf) := (p,q,F) -> (s := segre F; toList apply(p..q, i -> part(i,s)))

nonnull := x -> select(x, i -> i =!= null)

coerce := (F,G) -> (
     X := variety F;
     Y := variety G;
     if X === Y then return (F,G);
     AX := intersectionRing X;
     AY := intersectionRing Y;
     z := try 0_AX + 0_AY else error "expected abstract sheaves on compatible or equal varieties";
     if ring z === AX
     then (F, abstractSheaf(X,Rank => rank G, ChernClass => chern G)   )
     else (   abstractSheaf(Y,Rank => rank F, ChernClass => chern F), G)
     )

AbstractSheaf ++ RingElement := 
AbstractSheaf + RingElement := AbstractSheaf => (F,n) -> (
     if degree n =!= {0} then error "expected a ring element of degree 0";
     abstractSheaf(variety F, ChernCharacter => ch F + n))
AbstractSheaf ++ QQ := 
AbstractSheaf + QQ := 
AbstractSheaf ++ ZZ := 
AbstractSheaf + ZZ := AbstractSheaf => (F,n) -> abstractSheaf(variety F, ChernCharacter => ch F + n)
RingElement + AbstractSheaf := 
RingElement ++ AbstractSheaf := AbstractSheaf => (n,F) -> (
     if degree n =!= {0} then error "expected a ring element of degree 0";
     abstractSheaf(variety F, ChernCharacter => n + ch F))
QQ ++ AbstractSheaf := 
QQ + AbstractSheaf := 
ZZ ++ AbstractSheaf := 
ZZ + AbstractSheaf := AbstractSheaf => (n,F) -> abstractSheaf(variety F, ChernCharacter => n + ch F)

AbstractSheaf ++ AbstractSheaf :=
AbstractSheaf + AbstractSheaf := AbstractSheaf => (
     (F,G) -> abstractSheaf nonnull (
	  variety F, Rank => rank F + rank G,
	  ChernCharacter => ch F + ch G,
	  if F.cache.?ChernClass and G.cache.?ChernClass then 
	    ChernClass => F.cache.ChernClass * G.cache.ChernClass
	  )) @@ coerce

adams = method()
adams(ZZ,RingElement) := RingElement => (k,ch) -> (
     d := first degree ch;
     sum(0 .. d, i -> k^i * part_i ch))
adams(ZZ,AbstractSheaf) := AbstractSheaf => (k,E) -> abstractSheaf nonnull (variety E, Rank => rank E, 
     ChernCharacter => adams(k, ch E),
     if E.cache.?ChernClass then ChernClass => adams(k, E.cache.ChernClass)
     )
dual AbstractSheaf := AbstractSheaf => {} >> o -> E -> adams(-1,E)

- AbstractSheaf := AbstractSheaf => E -> abstractSheaf(variety E, Rank => - rank E, ChernCharacter => - ch E)
AbstractSheaf - AbstractSheaf := AbstractSheaf => ((F,G) -> F + -G) @@ coerce
AbstractSheaf - RingElement := AbstractSheaf => (F,n) -> (
     if degree n =!= {0} then error "expected a ring element of degree 0";
     abstractSheaf(variety F, ChernCharacter => ch F - n))
AbstractSheaf - QQ := 
AbstractSheaf - ZZ := AbstractSheaf => (F,n) -> abstractSheaf(variety F, ChernCharacter => ch F - n)
RingElement - AbstractSheaf := AbstractSheaf => (n,F) -> (
     if degree n =!= {0} then error "expected a ring element of degree 0";
     abstractSheaf(variety F, ChernCharacter => n - ch F))
QQ - AbstractSheaf := 
ZZ - AbstractSheaf := AbstractSheaf => (n,F) -> abstractSheaf(variety F, ChernCharacter => n - ch F)

AbstractSheaf ** AbstractSheaf :=
AbstractSheaf * AbstractSheaf := AbstractSheaf => (
     (F,G) -> (
	  f := ch F;
	  g := ch G;
	  X := variety F;
	  if f == 1 then G
	  else if g == 1 then F
	  else abstractSheaf(X, ChernCharacter => part(0,dim X,f*g)))
     ) @@ coerce

Hom(AbstractSheaf, AbstractSheaf) := AbstractSheaf => (F,G) -> dual F ** G

det AbstractSheaf := AbstractSheaf => opts -> (F) -> abstractSheaf(variety F, Rank => 1, ChernClass => 1 + part(1,ch F))

computeWedges = (n,A,d) -> (
     -- compute the chern characters of wedge(i,A), for i = 0..n, given a chern character, truncating above degree d
     wedge := new MutableList from splice{0..n};
     wedge#0 = 1_(ring A);
     wedge#1 = A;
     for p from 2 to n do
	  wedge#p = 1/p * sum for m from 0 to p-1 list (-1)^(p-m+1) * part(0,d,wedge#m * adams(p-m,A));
     toList wedge
     )

exteriorPower(ZZ, AbstractSheaf) := AbstractSheaf => opts -> (n,E) -> (
     -- wedge is an array 0..n of the chern characters of the exerior 
     -- powers of E.  The last one is what we want.

     -- this line of code is incorrect for virtual bundles:
     -- if 2*n > rank E then return det(E) ** dual exteriorPower(rank E - n, E);

     wedge := computeWedges(n,ch E,dim variety E);
     abstractSheaf(variety E, ChernCharacter => wedge#n)
     )

exteriorPower AbstractSheaf := AbstractSheaf => opts -> (E) -> (
     -- really only makes sense if E is "effective", but it's useful, anyway
     sum for i from 0 to rank E list (-1)^i * exteriorPower(i,E)
     )

symmetricPower(RingElement, AbstractSheaf) := AbstractSheaf => (n,F) -> (
     X := variety F;
     A := intersectionRing X;
     try n = promote(n,A);
     if not instance(n,A) then error "expected an element in the intersection ring of the variety";
     if not isHomogeneous n or degree n =!= {0} then error "expected homogeneous element of degree 0";
     -- This uses Grothendieck-Riemann-Roch, together with the fact that
     -- f_!(OO_PF(n)) = f_*(symm(n,F)), since the higher direct images are 0.
     h := local h;
     PF := projectiveBundle(F, VariableNames => h);
     f := PF.StructureMap;
     abstractSheaf(X, f_*(part(0,dim PF,ch OO_PF(n) * todd f))))

symmetricPower(ZZ, AbstractSheaf) := 
symmetricPower(QQ, AbstractSheaf) := AbstractSheaf => (n,E) -> (
     d := dim variety E;
     A := ch E;
     wedge := computeWedges(n,A,d);
     symms := new MutableList from splice{0..n};
     symms#0 = 1_(ring A);
     symms#1 = A;
     for p from 2 to n do (
	  r := min(p, rank E);
	  symms#p = sum for m from 1 to r list (-1)^(m+1) * part(0,d,wedge#m * symms#(p-m));
	  );
     abstractSheaf(variety E, ChernCharacter => symms#n)
     )

schur = method(TypicalValue => AbstractSheaf)
schur(List, AbstractSheaf) := (p,E) -> (
     -- Make sure that p is a monotone descending sequence of non-negative integers
     --q := conjugate new Partition from p;
     p = splice p;
     q := p;
     n := sum p;
     R := symmRing n;
     wedges := computeWedges(n,ch E,dim variety E);
     J := jacobiTrudi(q,R); -- so the result will be a poly in the wedge powers
     F := map(ring ch E, R, join(apply(splice{0..n-1}, i -> R_i => wedges#(i+1)), 
	                         apply(splice{n..2*n-1}, i -> R_i => 0)));
     abstractSheaf(variety E, ChernCharacter => F J))

schubertCycle = method(TypicalValue => RingElement)
FlagBundle _ Sequence := FlagBundle _ List := RingElement => (F,s) -> schubertCycle(s,F)
giambelli =  (r,E,b) -> (
     p := matrix for i from 0 to r-1 list for j from 0 to r-1 list chern(b#i-i+j,E); -- Giambelli's formula, also called Jacobi-Trudi
     if debugLevel > 15 then stderr << "giambelli : " << p << endl;
     det p
     )
listtoseq = (r,b) -> toSequence apply(#b, i -> r + i - b#i)
seqtolist = (r,b) ->            apply(#b, i -> r + i - b#i)
dualpart  = (r,b) -> splice for i from 0 to #b list ((if i === #b then r else b#(-i-1)) - (if i === 0 then 0 else b#-i)) : #b - i

schubertCycle(Sequence,FlagBundle) := (a,X) -> (
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
schubertCycle(List,FlagBundle) := (b,X) -> (
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

sectionZeroLocus = method(TypicalValue => AbstractVariety)
sectionZeroLocus AbstractSheaf := (F) -> (
     -- adapted from Schubert's bundlesection
     X := variety F;
     A := intersectionRing X;
     classZ := ctop F;
     B := A[Join=>false];		      -- a way to get a copy of A
     Z := abstractVariety(dim X - rank F, B);
     pullback := method();
     pullback ZZ := pullback QQ := pullback A := r -> promote(r,B);
     pullback AbstractSheaf := E -> (
	  if variety E =!= X then "pullback: variety mismatch";
	  abstractSheaf(Z, Rank => rank E, ChernClass => pullback chern E));
     pushforward := method();
     pushforward ZZ := pushforward QQ := r -> pushforward promote(r,B);
     pushforward B := b -> lift(b,A) * classZ;
     i := Z.StructureMap = new AbstractVarietyMap from {
     	  symbol source => Z,
     	  symbol target => X,
     	  PullBack => pullback,
     	  PushForward => pushforward,
	  TangentBundle => abstractSheaf(Z, ChernCharacter => - ch F)
	  };
     pushforward AbstractSheaf := E -> abstractSheaf(X,ChernCharacter => pushforward (ch E * todd i));
     integral B := m -> integral( lift(m,A) * classZ );
     if X.?TangentBundle then Z.TangentBundle = abstractSheaf(Z, ChernCharacter => ch tangentBundle X - ch F);
     Z)

degeneracyLocus2 = method(TypicalValue => RingElement)
degeneracyLocus2(ZZ,AbstractSheaf,AbstractSheaf) := (k,B,A) -> (
     X := variety B;
     if X =!= variety A then error "expected sheaves on the same variety";
     m := rank A;
     n := rank B;
     part( (m-k)*(n-k), ch schur( { n - k : m - k }, B - A )))

degeneracyLocus = method(TypicalValue => AbstractVariety)
degeneracyLocus(ZZ,AbstractSheaf,AbstractSheaf) := (k,B,A) -> (
     X := variety B;
     if X =!= variety A then error "expected sheaves on the same variety";
     m := rank A;
     n := rank B;
     G := flagBundle({m-k,k},A);
     S := first G.Bundles;
     sectionZeroLocus Hom(S,(G/X)^* B))

kernelBundle = method(TypicalValue => AbstractVariety)
kernelBundle(ZZ,AbstractSheaf,AbstractSheaf) := (k,B,A) -> (
     X := variety A;
     Z := degeneracyLocus(k,B,A);
     G := target Z.StructureMap;
     S := first G.Bundles;
     K := (Z/G)^* S;
     Z.StructureMap = Z/X;
     K)

beginDocumentation()
multidoc get (currentFileDirectory | "Schubert2/doc")
undocumented {
     (tangentBundle,FlagBundle),
     (symmetricPower,QQ,AbstractSheaf),
     (symmetricPower,ZZ,AbstractSheaf),
     (net,ChernClassVariableTable),
     (net,AbstractSheaf),
     (net,AbstractVariety),
     (net,AbstractVarietyMap),
     (net,ChernClassVariable),
     (net,FlagBundle),
     (baseName,ChernClassVariable),
     (expression,ChernClassVariable),
     (baseName,AbstractSheaf),
     (baseName,ChernClassVariableTable),
     (toString,AbstractSheaf),
     (toString,AbstractVariety),
     (toString,AbstractVarietyMap),
     (toString,ChernClassVariable),
     (toString,FlagBundle)
     }
TEST /// input (Schubert2#"source directory"|"Schubert2/demo.m2") ///
TEST /// input (Schubert2#"source directory"|"Schubert2/demo2.m2") ///
TEST /// input (Schubert2#"source directory"|"Schubert2/demo3.m2") ///
TEST /// input (Schubert2#"source directory"|"Schubert2/test-dan.m2") ///
TEST /// input (Schubert2#"source directory"|"Schubert2/test2-dan.m2") ///
TEST /// input (Schubert2#"source directory"|"Schubert2/blowup-test.m2") ///
TEST /// input (Schubert2#"source directory"|"Schubert2/BrillNoether-test.m2") ///
TEST /// input (Schubert2#"source directory"|"Schubert2/SymmetricProduct-test.m2") ///

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages PACKAGES=Schubert2 all check-Schubert2 RemakeAllDocumentation=true RerunExamples=true RemakePackages=true"
-- End:
