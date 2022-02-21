export {
    "setupCotangent",
    "tautoClass",
    "sClass", "stableClass", "segreClass", "chernClass", "schubertClass",
    "sClass'", "stableClass'", "segreClass'", "chernClass'", "schubertClass'",
    "restrict", "basisCoeffs",
    "pushforwardToPoint", "pushforwardToPointFromCotangent", "zeroSection", "dualZeroSection", "canonicalClass",
    "Presentation", "Borel", "EquivLoc",
    "inversion",
    "Partial"
    };

cotOpts := opts ++ { Presentation => EquivLoc }

debug Core -- to use basering, generatorSymbols, frame

-- labeling of classes
AryString = new Type of List;
new AryString from String := (T,s) -> apply(ascii s,i->i-48);
texMath AryString := s -> concatenate between("\\,",apply(s,x -> if class x === String then x else texMath x))
net AryString := toString AryString := s -> concatenate apply(s,toString) -- what about multinumbers???
toExternalString AryString := s -> toExternalString toString s
-- inversion number of a string
inversion = method()
inversion AryString := p -> sum(#p-1,i->sum(i+1..#p-1,j->if p_i>p_j then 1 else 0))
inversion String := s -> inversion new AryString from s

-- a simple function that seems like it should already exist
basisCoeffs = x -> lift(last coefficients(x, Monomials => basis ring x),(ring x).basering)

-- next two functions should be memoized
elem := (i,vrs) -> sum(subsets(vrs,i), product);
expandElem := (P,vrs,els) -> (
    if P == 0 then return 0;
    c := coefficients(P,Variables=>vrs);
    M := c#0_(0,0); C := c#1_(0,0);
    e := append((first exponents M)_(apply(vrs,index)),0);
    ee := apply(#vrs, i -> e#i - e#(i+1));
    if any(ee, i->i<0) then error "nonsymmetric polynomial";
    Q := P - C * product(#vrs, i -> (elem(i+1,vrs))^(ee#i));
    sub(C,ring first els) * product(#vrs, i -> (els#i)^(ee#i)) + expandElem(Q,vrs,els)
    )

-- automate promotion
promoteFromMap = method()
promoteFromMap (Ring,Ring,RingMap) := (R,S,f) -> (
    promote(R,S) := (a,S1) -> f a;
    promote(Matrix,R,S) :=
    promote(MutableMatrix,R,S) := -- doesn't work, cf https://github.com/Macaulay2/M2/issues/2192
    promote(Module,R,S) := (M,R1,S1) -> f M;
--    promote(List,R,S) := (L,R1,S1) -> f\L; -- TODO put back!!!!!!!!!!
    S.baseRings = prepend(R,S.baseRings); -- temporary -- until promotability test improved in enginering.m2
    )
promoteFromMap (Ring,Ring) := (R,S) -> promoteFromMap(R,S,map(S,R))

tautoClass = method(Dispatch=>{Thing,Thing,Type},Options=>true); -- "Chern classes" -- renamed tautoClass to avoid confusion with motivic classes
zeroSection = method(Dispatch=>{Type},Options=>true) -- note the {}
dualZeroSection = method(Dispatch=>{Type},Options=>true) -- note the {}
canonicalClass = method(Dispatch=>{Type},Options=>true) -- note the {}
zeroSectionInv = method(Dispatch=>{Type},Options=>true) -- internal use only
segreClass = method(Dispatch=>{Thing,Type},Options=>true)
sClass = method(Dispatch=>{Thing,Type},Options=>true)
stableClass = method(Dispatch=>{Thing,Type},Options=>true)
chernClass = method(Dispatch=>{Thing,Type},Options=>true)
schubertClass = method(Dispatch=>{Thing,Type},Options=>true)
sClass' = method(Dispatch=>{Thing,Type},Options=>true)
stableClass' = method(Dispatch=>{Thing,Type},Options=>true)
segreClass' = method(Dispatch=>{Thing,Type},Options=>true)
chernClass' = method(Dispatch=>{Thing,Type},Options=>true)
schubertClass' = method(Dispatch=>{Thing,Type},Options=>true)

-- internal use only. TODO absorb in closure?
sClasses = method(Dispatch=>{Type},Options=>true)
schubertClasses = method(Dispatch=>{Type},Options=>true)
schubertClasses' = method(Dispatch=>{Type},Options=>true)

-- for internal use, equiv loc presentation only
weights = method(Dispatch=>{Type})
cotweights = method(Dispatch=>{Type})

-- the methods below are defined for appropriate rings by setup
-- the defs below are just apply-type

-- restriction to fixed points
restrict = method(Dispatch => {Thing,Type})
restrict Matrix := m -> matrix apply(flatten entries m,restrict) -- only for one-row matrices
restrict (Matrix,RingElement) := (m,X) -> matrix apply(flatten entries m,x->restrict(x,X)) -- only for one-row matrices

-- pushforward
pushforwardToPoint=method(); -- pushforward to a point from K(G/P)
pushforwardToPointFromCotangent=method(); -- pushforward to a point from K(T^*(G/P))

-- common rings
q := getSymbol "q"; zbar := getSymbol "zbar";
FK_-1 = frac(factor(ZZ (monoid[q,zbar,DegreeRank=>0]))); -- same as FK_1, really but diff variable name
FK_0 = frac(factor(ZZ (monoid[q,DegreeRank=>0])));
promoteFromMap(FK_0,FK_-1);

h := getSymbol "h"; ybar := getSymbol "ybar";
FH_-1 = frac(factor(ZZ (monoid[h,ybar]))); -- same as FH_1, really but diff variable name
FH_0 = frac(factor(ZZ (monoid[h])));
promoteFromMap(FH_0,FH_-1);

defineFK = n -> (
    if not FK#?n then (
        z := getSymbol "z"; -- q := getSymbol "q";
        FK_n = frac(factor(ZZ (monoid[q,z_1..z_n,DegreeRank=>0,MonomialOrder=>{Weights=>{n+1:1},RevLex}])));
        promoteFromMap(FK_0,FK_n);
        );
    FK#n
    )

defineFH = n -> (
    if not FH#?n then (
        y := getSymbol "y"; -- h := getSymbol "h";
        FH_n = frac(factor(ZZ (monoid[h,y_1..y_n,MonomialOrder=>{Weights=>{n+1:1},Weights=>{1,n:0},RevLex}])));
        promoteFromMap(FH_0,FH_n);
        );
    FH#n
    )

BBs = new IndexedVariableTable;

defineB = (FF,n,Kth,Equiv) -> ( -- TODO remove FF
    if not BBs#?(n,Kth,Equiv) then (
	x := getSymbol "x";
	BB0 := FF(monoid(splice[x_1..x_n, if Kth then DegreeRank=>0 else (MonomialOrder=>{Weights=>{n:1},RevLex},DegreeRank=>1)])); -- in terms of Chern roots
	J := ideal apply(1..n,k->elem(k,gens BB0)
            -if Equiv then elem(k,drop(gens FF,1)) else if Kth then binomial(n,k) else 0);
	BB := BB0/J;
	BBs#(n,Kth,Equiv) = BB;
	);
    BBs#(n,Kth,Equiv)
    )

-- diagonal algebra
DiagonalAlgebra = new Type of Type;
DiagonalAlgebra List := (D,l) -> new D from {map(D.Module,(ring D)^1,apply(splice l, i -> {i}))}; -- cannot be new D from List because would break existing Vector code
new DiagonalAlgebra from Module := (X,M) -> (
    D := new DiagonalAlgebra of Vector from hashTable { global Module => M };
    new D from Vector := (D,v) -> (
	if class v =!= M then try (
	    v = promote(v,ring M);
	    assert(class v === M);
	    ) else error "wrong type of vector";
	v);
    new D from Number :=
    new D from RingElement := (D,x) -> D toList(rank M:x);
    vector D := d -> new M from d;
    matrix D := opts -> d -> diagonalMatrix entries d;
    D * Vector := (x,v) -> if class v === M then matrix x * v else error "wrong vector";
    D * Matrix := (x,m) -> if target m === M then matrix x * m else error "wrong target";
    D * D := (v,w) -> D apply(entries v,entries w,(x,y)->x*y); -- componentwise product
    D ^ ZZ := (v,n) -> D apply(entries v, a -> a^n); -- componentwise power
    D + Number := D + RingElement := (v,x) -> v + new D from x;
    Number + D := RingElement + D := (x,v) -> v + new D from x;
    D - Number := D - RingElement := (v,x) -> v - new D from x;
    Number - D := RingElement - D := (x,v) -> - v + new D from x;
    D == Vector := Vector == D := (x,y) -> x#0 == y#0;
    D == Number := (x,n) -> x == new D from n;
    Number == DD := (n,x) -> x == new D from n;
    D)
ring DiagonalAlgebra := D -> ring D.Module;
rank DiagonalAlgebra := D -> rank D.Module;
expression DiagonalAlgebra := D -> if hasAttribute(D,ReverseDictionary) then return expression getAttribute(D,ReverseDictionary) else (expression DiagonalAlgebra) D.Module;
net DiagonalAlgebra := D -> net expression D;
toString DiagonalAlgebra := D -> toString expression D;
texMath DiagonalAlgebra := D -> texMath expression D;
html DiagonalAlgebra := lookup(html,Thing);

-- ex: D=new DiagonalAlgebra from ZZ^3; x=D {1,2,3}; x^2-x+1

-- main function: set up everything
setupCotangent = cotOpts >> curCotOpts -> dims0 -> (
    if #dims0 === 0 or unique dims0 === {0} then error "Please specify nonzero dimensions";
    -- "global" parameters
    dims := if first dims0 == 0 then dims0 else prepend(0,dims0);
    n := last dims;
    d := #dims - 2; -- # steps - 2 since includes trivial first and last
    subs := s -> apply(d+1,i->positions(s,j->j==i));
    dimdiffs := apply(d+1, i-> dims#(i+1)-dims#i);
    dimvar := sum(d+1,i->dims#i*dimdiffs#i); -- dimension of flag variety
    ω:=new AryString from splice apply(d+1, i->dimdiffs_i:i); -- list of fixed points
    I := unique permutations ω; -- unique? eww
    ind := method();
    ind AryString := i -> sum(#i,j->(d+1)^j*i_(#i-1-j));
    ind String := s -> ind new AryString from s;
    -- redefine default puzzle opts
    (frame puzzle)#0 = applyPairs((frame puzzle)#0,(k,v) -> (k,if curCotOpts#?k then curCotOpts#k else v));
    -- set up base ring and R-matrices
    if curCotOpts.Ktheory then (
	FF0 := FK_0;
	FF := if curCotOpts.Equivariant then defineFK n else FF0;
	V1:=FK_-1^(d+1); q:=FK_-1_0; zbar:=FK_-1_1;
	Rcnum0:=map(V1^**2,V1^**2,splice flatten table(d+1,d+1,(i,j)->
		if i==j then (i*(d+2),i*(d+2))=>1-q^2*zbar
		else ((i*(d+1)+j,j*(d+1)+i)=>q*(1-zbar),
                    (i*(d+1)+j,i*(d+1)+j)=>(1-q^2)* if i<j then 1 else zbar)));
	Rcz0:=map(V1^**2,V1^**2,splice flatten table(d+1,d+1,(i,j)->
		if i==j then (i*(d+2),i*(d+2))=>1
		else ((i*(d+1)+j,j*(d+1)+i)=>if i<j then 1-zbar^(-1) else 0,
                    (i*(d+1)+j,i*(d+1)+j)=>if i<j then 1 else zbar^(-1)))); -- note the annoying ^(-1)
	Rcz0':=map(V1^**2,V1^**2,splice flatten table(d+1,d+1,(i,j)->
		if i==j then (i*(d+2),i*(d+2))=>1
		else ((i*(d+1)+j,j*(d+1)+i)=>if i<j then 1-zbar^(-1) else 0,
                    (i*(d+1)+j,i*(d+1)+j)=>if i<j then zbar^(-1) else 1))); -- note the annoying ^(-1)
	Rcden0:=1-q^2*zbar;
	Rc0 := 1/Rcden0 * Rcnum0;
	-- TODO rewrite next 4 statements better
	Rc := (z1,z2) -> (map(ring z2,FK_-1,{FF_0,z2/z1}))Rc0;
	Rcnum := (z1,z2) -> (map(ring z2,FK_-1,{FF_0,z2*z1^(-1)}))Rcnum0;
	Rcden := (z1,z2) -> (map(ring z2,FK_-1,{FF_0,z2*z1^(-1)}))Rcden0;
	Rcz := (z1,z2) -> (map(ring z2,FK_-1,{FF_0,z2*z1^(-1)}))Rcz0;
	Rcz' := (z1,z2) -> (map(ring z2,FK_-1,{FF_0,z2*z1^(-1)}))Rcz0';
	) else (
	FF0 = FH_0;
	FF = if curCotOpts.Equivariant then defineFH n else FF0;
	V1=FH_-1^(d+1); h:=FH_-1_0; ybar:=FH_-1_1;
	Rcnum0=map(V1^**2,V1^**2,splice flatten table(d+1,d+1,(i,j)->
		if i==j then (i*(d+2),i*(d+2))=>h-ybar
		else ((i*(d+1)+j,j*(d+1)+i)=>ybar,
                    (i*(d+1)+j,i*(d+1)+j)=>h)));
	Rcz0=map(V1^**2,V1^**2,splice flatten table(d+1,d+1,(i,j)->
		if i==j then (i*(d+2),i*(d+2))=>1
		else ((i*(d+1)+j,j*(d+1)+i)=>if i<j then ybar else 0,
                    (i*(d+1)+j,i*(d+1)+j)=>1)));
	Rcden0=h-ybar;
	Rc0 = 1/Rcden0 * Rcnum0;
	-- TODO rewrite next 4 statements better
	Rc = (x1,x2) -> (map(ring x2,FH_-1,{FF_0,x2-x1}))Rc0;
	Rcnum = (x1,x2) -> (map(ring x2,FH_-1,{FF_0,x2-x1}))Rcnum0;
	Rcden = (x1,x2) -> (map(ring x2,FH_-1,{FF_0,x2-x1}))Rcden0;
	Rcz = Rcz' = (x1,x2) -> (map(ring x2,FH_-1,{FF_0,x2-x1}))Rcz0;
        );
    if curCotOpts.Presentation === Borel then (
	BB := defineB(FF,n,curCotOpts.Ktheory,curCotOpts.Equivariant);
	if curCotOpts.Equivariant then promoteFromMap(FF0,BB,map(BB,FF0,{FF_0})); -- TODO move elsewhere
	x := getSymbol "x";
	-- Chern classes
	inds := splice apply(d+1, i -> apply(1..dimdiffs#i,j->(j,i)));
	v := (j,i) -> x_(j,toList(dims#i+1..dims#(i+1))); -- variable name
	e := (j,i) -> elem(j,apply(dims#i..dims#(i+1)-1,k->BB_k)); -- expression in terms of Chern roots
	args := v\inds;
	if curCotOpts.Ktheory then (
	    args = append(args,DegreeRank=>0);
	    wgts := apply(d+1,i->Weights=>apply(d+1,j->dimdiffs#j:(if j==i then 1 else 0)));
	    args = append(args, MonomialOrder=>wgts); -- a sort of RevLex
	    ) else (
	    degs := splice apply(d+1,i->1..dimdiffs#i);
	    args = append(args, Degrees=>degs);
	    wgts = apply(splice apply(d+1,i->reverse(dims#i..dims#(i+1)-1)),i->Weights=>apply(#inds,j->if j==i then -1 else 0));
	    args = append(args, MonomialOrder=>prepend(Weights=>degs,wgts)); -- a sort of GRevLex but with different ordering of variables
	    );
	R1 := FF monoid new Array from args;
	f := map(BB,R1,e\inds);
	AA := R1 / kernel f;
	if curCotOpts.Equivariant then promoteFromMap(FF0,AA,map(AA,FF0,{FF_0}));
	promoteFromMap(AA,BB,f*map(R1,AA));
	-- reverse transformation
	lift(Module,BB,AA) := opts -> (v,b,AA) -> vector apply(entries v,x->lift(x,AA));
	lift(Matrix,BB,AA) := opts -> (m,b,AA) -> matrix applyTable(entries m,x->lift(x,AA));
	lift (BB,AA) := opts -> (b,AA) -> (
	    if d == n-1 then return (map(AA,BB,gens AA)) b; -- special case of full flag
	    AB := FF monoid (BB.generatorSymbols | AA.generatorSymbols); -- no using it
	    b = sub(b,AB);
	    -- scan(d+1,i->b=expandElem(b,toList(AB_(dims#i)..AB_(dims#(i+1)-1)),toList(AB_(n+dims#i)..AB_(n+dims#(i+1)-1))));
	    -- fails because of https://github.com/Macaulay2/M2/issues/2020
	    v := seq -> apply(toList seq, j -> AB_j);
	    scan(d+1,i->b=expandElem(b,v(dims#i..dims#(i+1)-1),v(n+dims#i..n+dims#(i+1)-1)));
	    sub(b,AA)
	    );
	--
	tautoClass (ZZ,ZZ,AA) := { Partial => true} >> o -> (j,i,AA) -> if o.Partial then AA_(dims#i+j-1) else e (j,i);
	zeroSection AA := { Partial => true} >> o -> (cacheValue (zeroSection,o.Partial)) (if o.Partial then AA -> lift(zeroSection(AA,Partial=>false),AA)
	    else if curCotOpts.Ktheory then
	    AA -> product(n,j->product(n,k->if ω#j<ω#k then 1-FF_0^2*BB_j*BB_k^(-1) else 1))
	    else AA -> product(n,j->product(n,k->if ω#j<ω#k then FF_0-BB_j+BB_k else 1)));
	dualZeroSection AA := { Partial => true} >> o -> (cacheValue (dualZeroSection,o.Partial)) (if o.Partial then AA -> lift(dualZeroSection(AA,Partial=>false),AA)
	    else if curCotOpts.Ktheory then
	    AA -> product(n,j->product(n,k->if ω#j<ω#k then 1-FF_0^-2*BB_k*BB_j^(-1) else 1))
	    else AA -> product(n,j->product(n,k->if ω#j<ω#k then -FF_0+BB_j-BB_k else 1)));
	if curCotOpts.Ktheory then canonicalClass AA :=  { Partial => true} >> o -> (cacheValue (canonicalClass,o.Partial)) (if o.Partial then AA -> lift(canonicalClass(AA,Partial=>false),AA)
	    else AA -> product(n,j->product(n,k->if ω#j<ω#k then BB_k*BB_j^(-1) else 1)));
	zeroSectionInv AA := { Partial => true } >> o -> (cacheValue (zeroSectionInv,o.Partial)) (AA -> (zeroSection(AA,o))^(-1));
	-- segre Classes TODO rethink: closure?
	sClasses AA := {Partial=>true} >> o -> (cacheValue (sClasses,o.Partial)) (if o.Partial then AA -> lift(sClasses(AA,Partial=>false),AA)
		else AA -> (
		-- monodromy matrix
		V:=BB^(d+1);
		W:=V^**n;
		Z:=map(BB^1,W,{{rank W-1:0,1}});
		scan(reverse(0..dims#d-1),i->( -- not 0..n-1: slight optimization: don't do trivial rows
			T:=map(V^**(n+1),V^**(n+1),1);
			scan(n,j->T=T*(map(V^**j,V^**j,1)**(Rcnum (
					if curCotOpts.Equivariant then FF_(j+1) else if curCotOpts.Ktheory then 1 else 0,BB_i)
				    )**map(V^**(n-1-j),V^**(n-1-j),1)));
			--print i;
			Z=Z*submatrix(T,{(rank W)*ω_i..(rank W)*(ω_i+1)-1},apply(rank W,i->i*(d+1)+d));
			--print Z;
			));
		scan(dims#d,i->scan(n,j-> Z = Z*(Rcden(
				if curCotOpts.Equivariant then FF_(j+1) else if curCotOpts.Ktheory then 1 else 0,BB_i))^(-1)));
		Z
		));
	sClass (List,AA) := {Partial=>true} >> o -> (L,AA) -> (sClasses(AA,o))_(ind\L);
	sClass (String,AA) :=
	sClass (AryString,AA) := {Partial=>true} >> o -> (i,AA) -> (sClasses(AA,o))_(0,ind i);
	segreClass (List,AA) := {Partial=>true} >> o -> (L,AA) -> matrix { apply(L,i->segreClass(i,AA,o)) };
	segreClass (String,AA) :=
	segreClass (AryString,AA) := {Partial=>true} >> o -> (i,AA) -> (if curCotOpts.Ktheory then FF_0 else -1)^(inversion i)*sClass(i,AA,o);

	stableClass (List,AA) :=
	stableClass (String,AA) :=
	stableClass (AryString,AA) := {Partial=>true} >> o -> (i,AA) -> zeroSection(AA,o) * sClass(i,AA,o);

	chernClass (List,AA) :=
	chernClass (String,AA) :=
	chernClass (AryString,AA) := {Partial=>true} >> o -> (i,AA) -> dualZeroSection(AA,o) * segreClass(i,AA,o);
	-- Schubert classes
	schubertClasses AA := {Partial=>true} >> o -> (cacheValue (schubertClasses,o.Partial)) (if o.Partial then AA -> lift(schubertClasses(AA,Partial=>false),AA)
		else AA -> (
		-- monodromy matrix
		V:=BB^(d+1);
		W:=V^**n;
		Z:=map(BB^1,W,{{rank W-1:0,1}});
		scan(reverse(0..dims#d-1),i->( -- not 0..n-1: slight optimization: don't do trivial rows
			T:=map(V^**(n+1),V^**(n+1),1);
			scan(n,j->T=T*(map(V^**j,V^**j,1)**(Rcz (
					if curCotOpts.Equivariant then FF_(j+1) else if curCotOpts.Ktheory then 1 else 0,BB_i)
				    )**map(V^**(n-1-j),V^**(n-1-j),1)));
			--print i;
			Z=Z*submatrix(T,{(rank W)*ω_i..(rank W)*(ω_i+1)-1},apply(rank W,i->i*(d+1)+d));
			--print Z;
			));
		Z
		));
	schubertClass (List,AA) := {Partial=>true} >> o -> (L,AA) -> (schubertClasses(AA,o))_(ind\L);
	schubertClass (String,AA) :=
	schubertClass (AryString,AA) := {Partial=>true} >> o -> (i,AA) -> (schubertClasses(AA,o))_(0,ind i);
	-- duality
	du1 := if curCotOpts.Ktheory then prepend(FF_0^-1,apply(numgens FF-1,i->FF_(numgens FF-1-i)^-1))
	    else prepend(-FF_0,apply(numgens FF-1,i->-FF_(numgens FF-1-i)));
	du2 := apply(gens BB,x->if curCotOpts.Ktheory then x^-1 else -x);
	du := map(BB,BB,du2|du1);
	sClass' (List,AA) :=
	sClass' (String,AA) :=
	sClass' (AryString,AA) := {Partial=>true} >> o -> (i,AA) -> (
	    x := du sClass(if class i === List then reverse\i else reverse i,AA,Partial=>false);
	    if o.Partial then lift(x,AA) else x
	    );
	-- compared to Mihalcea, missing a (-t)^-dimvar; compared to dual of chernClass, missing q^#
	segreClass' (List,AA) :=
	segreClass' (String,AA) :=
	segreClass' (AryString,AA) := {Partial=>true} >> o -> (i,AA) -> (
	    x := du segreClass(if class i === List then reverse\i else reverse i,AA,Partial=>false);
	    if o.Partial then lift(x,AA) else x
	    );
	-- compared to Mihalcea, missing a (-t)^-dimvar; compared to dual of segreClass, missing q^#
	chernClass' (List,AA) :=
	chernClass' (String,AA) :=
	chernClass' (AryString,AA) := {Partial=>true} >> o -> (i,AA) -> dualZeroSection(AA,o) * segreClass'(i,AA,o);
	stableClass' (List,AA) := {Partial=>true} >> o -> (L,AA) -> matrix { apply(L,i->stableClass'(i,AA,o)) };
	stableClass' (String,AA) :=
	stableClass' (AryString,AA) := {Partial=>true} >> o -> (i,AA) -> (if curCotOpts.Ktheory then FF_0 else -1)^(2*dimvar-inversion i)*chernClass'(i,AA,o);
	schubertClasses' AA := {Partial=>true} >> o -> (cacheValue (schubertClasses',o.Partial)) (if o.Partial then AA -> lift(schubertClasses'(AA,Partial=>false),AA)
		else AA -> (
		-- monodromy matrix
		V:=BB^(d+1);
		W:=V^**n;
		Z:=map(BB^1,W,{{rank W-1:0,1}});
		scan(reverse(0..dims#d-1),i->( -- not 0..n-1: slight optimization: don't do trivial rows
			T:=map(V^**(n+1),V^**(n+1),1);
			scan(n,j->T=T*(map(V^**j,V^**j,1)**(Rcz' (
					if curCotOpts.Equivariant then FF_(n-j) else if curCotOpts.Ktheory then 1 else 0,BB_i) -- note reversed equiv params
				    )**map(V^**(n-1-j),V^**(n-1-j),1)));
			--print i;
			Z=Z*submatrix(T,{(rank W)*ω_i..(rank W)*(ω_i+1)-1},apply(rank W,i->i*(d+1)+d));
			--print Z;
			));
		Z
		));
	schubertClass' (List,AA) := {Partial=>true} >> o -> (L,AA) -> (schubertClasses'(AA,o))_(ind\reverse\L);
	schubertClass' (String,AA) :=
	schubertClass' (AryString,AA) := {Partial=>true} >> o -> (i,AA) -> (schubertClasses'(AA,o))_(0,ind reverse i);
	-- restriction to fixed points
	if curCotOpts.Equivariant then (
	    restrictMap := i -> map(FF,BB, apply(n,j->FF_((flatten subs i)#j+1)));
	    restrict (Number,AA) :=
	    restrict (AA,AA) :=
	    restrict (BB,AA) := (b,AA) -> vector apply(I,i->(restrictMap i) b); -- where is D?
	    restrict Number := restrict AA := restrict BB := b -> restrict(b,AA);
	    );
	-- pushforwards
	-- find element whose pushforward is nonzero
	local nzpf; -- index of nonzero pushforward basis element
	if curCotOpts.Ktheory then (
	    nzpf = 0;
	    ) else (
	    -- with normal ordering: product of det line bundles ^ dims of flags product(1..d,i->tautoClass(dimdiffs#i,i)^(dims#i))
	    -- with reverse ordering: product(1..d,i->tautoClass(dimdiffs#i,i)^(codims#i)) where codim#i = last dims - dims#i
	    nzpf = maxPosition flatten last degrees basis AA; -- we locate it by max degree
	    );
	pushforwardToPoint AA := a -> (basisCoeffs a)_(nzpf,0);
	pushforwardToPoint Number := pushforwardToPoint RingElement := r -> pushforwardToPoint promote(r,AA);
	pushforwardToPoint Matrix := m -> matrix applyTable(entries m,pushforwardToPoint); -- here, not outside
	pushforwardToPointFromCotangent AA := a -> pushforwardToPoint (zeroSectionInv AA * a);
	pushforwardToPointFromCotangent Number := pushforwardToPoint RingElement := r -> pushforwardToPointFromCotangent promote(r,AA);
	pushforwardToPointFromCotangent Matrix := m -> matrix applyTable(entries m,pushforwardToPointFromCotangent);
	--
	tautoClass (ZZ,ZZ,BB) := {Partial=>false} >> o -> (j,i,BB) -> tautoClass(j,i,AA,o);
	tautoClass (ZZ,ZZ) := {Partial=>false} >> o -> (j,i) -> tautoClass(j,i,AA,o);
	zeroSection BB := {Partial=>false} >> o -> BB -> zeroSection(AA,o);
	installMethod(zeroSection, {Partial=>false} >> o->()->zeroSection(AA,o));
	dualZeroSection BB := {Partial=>false} >> o -> BB -> dualZeroSection(AA,o);
	installMethod(dualZeroSection,{Partial=>false} >> o->()->dualZeroSection(AA,o));
	canonicalClass BB := {Partial=>false} >> o -> BB -> canonicalClass(AA,o);
	installMethod(canonicalClass,{Partial=>false} >> o->()->canonicalClass(AA,o));
	zeroSectionInv BB := {Partial=>false} >> o -> BB -> zeroSectionInv(AA,o);
	sClass (List,BB) :=
	sClass (String,BB) :=
	sClass (AryString,BB) := {Partial=>false} >> o -> (i,BB) -> sClass(i,AA,o);
	sClass Thing := {Partial=>false} >> o -> i -> sClass(i,AA,o);
	segreClass (List,BB) :=
	segreClass (String,BB) :=
	segreClass (AryString,BB) := {Partial=>false} >> o -> (i,BB) -> segreClass(i,AA,o);
	segreClass Thing := {Partial=>false} >> o -> i -> segreClass(i,AA,o);
	stableClass (List,BB) :=
	stableClass (String,BB) :=
	stableClass (AryString,BB) := {Partial=>false} >> o -> (i,BB) -> stableClass(i,AA,o);
	stableClass Thing := {Partial=>false} >> o -> i -> stableClass(i,AA,o);
	chernClass (List,BB) :=
	chernClass (String,BB) :=
	chernClass (AryString,BB) := {Partial=>false} >> o -> (i,BB) -> chernClass(i,AA,o);
	chernClass Thing := {Partial=>false} >> o -> i -> chernClass(i,AA,o);
	schubertClass (List,BB) :=
	schubertClass (String,BB) :=
	schubertClass (AryString,BB) := {Partial=>false} >> o -> (i,BB) -> schubertClass(i,AA,o);
	schubertClass Thing := {Partial=>false} >> o -> i -> schubertClass(i,AA,o);
	sClass' (List,BB) :=
	sClass' (String,BB) :=
	sClass' (AryString,BB) := {Partial=>false} >> o -> (i,BB) -> sClass'(i,AA,o);
	sClass' Thing := {Partial=>false} >> o -> i -> sClass'(i,AA,o);
	segreClass' (List,BB) :=
	segreClass' (String,BB) :=
	segreClass' (AryString,BB) := {Partial=>false} >> o -> (i,BB) -> segreClass'(i,AA,o);
	segreClass' Thing := {Partial=>false} >> o -> i -> segreClass'(i,AA,o);
	chernClass' (List,BB) :=
	chernClass' (String,BB) :=
	chernClass' (AryString,BB) := {Partial=>false} >> o -> (i,BB) -> chernClass'(i,AA,o);
	chernClass' Thing := {Partial=>false} >> o -> i -> chernClass'(i,AA,o);
	stableClass' (List,BB) := {Partial=>false} >> o -> (L,BB) -> matrix { apply(L,i->stableClass'(i,AA,o)) };
	stableClass' (String,BB) :=
	stableClass' (AryString,BB) := {Partial=>false} >> o -> (i,BB) -> stableClass'(i,AA,o);
	stableClass' Thing := {Partial=>false} >> o -> i -> stableClass'(i,AA,o);
	schubertClass' (List,BB) :=
	schubertClass' (String,BB) :=
	schubertClass' (AryString,BB) := {Partial=>false} >> o -> (i,BB) -> schubertClass'(i,AA,o);
	schubertClass' Thing := {Partial=>false} >> o -> i -> schubertClass'(i,AA,o);
	pushforwardToPoint BB := b -> pushforwardToPoint lift(b,AA);
	pushforwardToPointFromCotangent BB := b -> pushforwardToPoint (zeroSectionInv BB * b);
	--
	(AA,BB,FF,I)
	) else if curCotOpts.Presentation === EquivLoc then (
	if not curCotOpts.Equivariant then error "Equivariant localization requires Equivariant option";
	-- precompute Rcheck-matrices
	V:=FF^(d+1); Rcheck := new IndexedVariableTable; Rcheckz := new IndexedVariableTable; Rcheckz' := new IndexedVariableTable;
	scan(n-1,j->Rcheck_j = map(V^**j,V^**j,1)**(Rc (FF_(j+1),FF_(j+2)))**map(V^**(n-2-j),V^**(n-2-j),1));
	scan(n-1,j->Rcheckz_j = map(V^**j,V^**j,1)**(Rcz (FF_(j+1),FF_(j+2)))**map(V^**(n-2-j),V^**(n-2-j),1));
	scan(n-1,j->Rcheckz'_j = map(V^**j,V^**j,1)**(Rcz' (FF_(j+2),FF_(j+1)))**map(V^**(n-2-j),V^**(n-2-j),1)); -- order of variables is fixed later
	-- Module are immutable so can't use them. DiagonalAlgebra aren't
	M := FF^#I;
	D := new DiagonalAlgebra from M;
	fixedPoint := memoize( (Rcheck,i) -> ( -- this returns the restrictions to a given fixed point
		-- find first descent
		j:=position(0..n-2,k->i#k>i#(k+1));
		tau0:=new AryString from apply(n,k->if k==j then j+1 else if k==j+1 then j else k);
		tau:=map(FF,FF,prepend(FF_0,(drop(gens FF,1))_tau0));
		(tau (fixedPoint(Rcheck,i_tau0)))*Rcheck_j
		), {
		(Rcheck,ω) => transpose matrix ZZ^((d+1)^n)_(ind ω),
		(Rcheckz,ω) => transpose matrix ZZ^((d+1)^n)_(ind ω),
		(Rcheckz',ω) => transpose matrix ZZ^((d+1)^n)_(ind ω),
		} );
	-- segre & schubert classes
	sClass (List,D) := {} >> o -> (L,D) -> ( -- should I cacheValue?
		inds := ind \ L;
		map(M,FF^#L, apply(I,i->first entries (fixedPoint(Rcheck,i))_inds))
		);
	sClass (String,D) :=
	sClass (AryString,D) := {} >> o -> (i,D) -> (
	    indi:=ind i;
	    D apply(I,ii->(fixedPoint(Rcheck,ii))_(0,indi))
	    );
	sClass Thing := {} >> o -> i -> sClass(i,D);
	segreClass (String,D) :=
	segreClass (AryString,D) := {} >> o -> (i,D) -> (if curCotOpts.Ktheory then FF_0 else -1)^(inversion i)*sClass(i,D);
	segreClass (List,D) := {} >> o -> (L,D) -> (
	    q := if curCotOpts.Ktheory then FF_0 else -1;
	    sClass(L,D) * diagonalMatrix apply(L,i->q^(inversion i))
	    );
	segreClass Thing := {} >> o -> i -> segreClass(i,D);
	chernClass (String,D) :=
	chernClass (AryString,D) := {} >> o -> (i,D) -> dualZeroSection D * segreClass(i,D);
	chernClass (List,D) := {} >> o -> (L,D) -> matrix dualZeroSection D * segreClass(L,D);
	chernClass Thing := {} >> o -> i -> chernClass(i,D);
	stableClass (String,D) :=
	stableClass (AryString,D) := {} >> o -> (i,D) -> zeroSection D * sClass(i,D);
	stableClass (List,D) := {} >> o -> (L,D) -> matrix zeroSection D * sClass(L,D);
	stableClass Thing := {} >> o -> i -> stableClass(i,D);
	schubertClass (List,D) := {} >> o -> (L,D) -> ( -- should I cacheValue?
		inds := ind \ L;
		map(M,M, apply(I,i->first entries (fixedPoint(Rcheckz,i))_inds))
		);
	schubertClass (String,D) :=
	schubertClass (AryString,D) := {} >> o -> (i,D) -> (
	    indi:=ind i;
	    D apply(I,ii->(fixedPoint(Rcheckz,ii))_(0,indi))
	    );
	schubertClass Thing := {} >> o -> i -> schubertClass(i,D);
	if curCotOpts.Ktheory then (
	    zeroSection D := {} >> o -> (cacheValue zeroSection) (D -> D apply(I,i->product(n,j->product(n,k->if i#j<i#k then 1-FF_0^2*FF_(j+1)/FF_(k+1) else 1))));
	    zeroSectionInv D := {} >> o -> (cacheValue zeroSectionInv) (D -> D apply(I,i->product(n,j->product(n,k->if i#j<i#k then (1-FF_0^2*FF_(j+1)/FF_(k+1))^(-1) else 1))));
	    dualZeroSection D := {} >> o -> (cacheValue dualZeroSection) (D -> D apply(I,i->product(n,j->product(n,k->if i#j<i#k then 1-FF_0^-2*FF_(j+1)^-1*FF_(k+1) else 1))));
	    weights D := (cacheValue weights) (D -> map(FF^1,M, { apply(I,i->product(n,j->product(n,k->if i#j<i#k then (1-FF_(k+1)/FF_(j+1))^(-1) else 1))) }));
	    cotweights D := (cacheValue cotweights) (D -> map(FF^1,M, { apply(I,i->product(n,j->product(n,k->if i#j<i#k then (1-FF_(k+1)/FF_(j+1))^(-1)*(1-FF_0^2*FF_(j+1)/FF_(k+1))^(-1) else 1))) }));
	    canonicalClass D := {} >> o -> (cacheValue canonicalClass) (D -> D apply(I,i->product(n,j->product(n,k->if i#j<i#k then FF_(j+1)^-1*FF_(k+1) else 1))));
	    installMethod(canonicalClass,{}>>o->()->canonicalClass D);
	    ) else (
	    zeroSection D := {} >> o -> (cacheValue zeroSection) (D -> D apply(I,i->product(n,j->product(n,k->if i#j<i#k then FF_0-FF_(j+1)+FF_(k+1) else 1))));
	    zeroSectionInv D := {} >> o -> (cacheValue zeroSectionInv) (D -> D apply(I,i->product(n,j->product(n,k->if i#j<i#k then (FF_0-FF_(j+1)+FF_(k+1))^(-1) else 1))));
	    dualZeroSection D := {} >> o -> (cacheValue dualZeroSection) (D -> D apply(I,i->product(n,j->product(n,k->if i#j<i#k then -FF_0+FF_(j+1)-FF_(k+1) else 1))));
	    weights D := (cacheValue weights) (D -> map(FF^1,M, { apply(I,i->product(n,j->product(n,k->if i#j<i#k then (FF_(j+1)-FF_(k+1))^(-1) else 1))) }));
	    cotweights D := (cacheValue cotweights) (D -> map(FF^1,M, { apply(I,i->product(n,j->product(n,k->if i#j<i#k then (FF_(j+1)-FF_(k+1))^(-1)*(FF_0-FF_(j+1)+FF_(k+1))^(-1) else 1))) }));
	    );
	installMethod(zeroSection,{}>>o->()->zeroSection D);
	installMethod(dualZeroSection,{}>>o->()->dualZeroSection D);
	-- Chern classes of tautological bundles
	tautoClass (ZZ,ZZ,D) := {} >> o -> (j,i,AA) -> D apply(I,s->elem(j,apply((subs s)#i,k->FF_(k+1))));
	tautoClass (ZZ,ZZ) := {} >> o -> (j,i) -> tautoClass(j,i,D);
	-- pushforward to point
	pushforwardToPoint D := pushforwardToPoint Vector := m -> ((weights D)*m)_0;
	pushforwardToPoint Matrix := m -> (weights D)*m;
	pushforwardToPoint Number := pushforwardToPoint RingElement := r -> pushforwardToPoint promote(r,D);
	pushforwardToPointFromCotangent D := pushforwardToPointFromCotangent Vector := m -> ((cotweights D)*m)_0;
	pushforwardToPointFromCotangent Matrix := m -> (cotweights D)*m;
	pushforwardToPointFromCotangent Number := pushforwardToPointFromCotangent RingElement := r -> pushforwardToPointFromCotangent promote(r,D);
	-- duality
	du = map(FF,FF,if curCotOpts.Ktheory then prepend(FF_0^-1,apply(n,i->FF_(n-i)^-1)) else prepend(-FF_0,apply(n,i->-FF_(n-i))));
	star := apply(I,i->(j:=reverse i; position(I,i'->i'==j)));
	sClass' (List,D) := {} >> o -> (L,D) -> (du sClass(reverse\L,D))^star;
	sClass' (String,D) :=
	sClass' (AryString,D) := {} >> o -> (i,D) -> new D from (du sClass(reverse i,D))^star;
	sClass' Thing := {} >> o -> i -> sClass'(i,D);
	-- compared to Mihalcea, missing a (-t)^-D; compared to dual of chernClass, missing q^#
	segreClass' (List,D) := {} >> o -> (L,D) -> (du segreClass(reverse\L,D))^star;
	segreClass' (String,D) :=
	segreClass' (AryString,D) := {} >> o -> (i,D) -> new D from (du segreClass(reverse i,D))^star;
	segreClass' Thing := {} >> o -> i -> segreClass'(i,D);
	-- compared to Mihalcea, missing a (-t)^-D; compared to dual of segreClass, missing q^#
	chernClass' (String,D) :=
	chernClass' (AryString,D) := {} >> o -> (i,D) -> dualZeroSection D * segreClass'(i,D);
	chernClass' (List,D) := {} >> o -> (L,D) -> matrix dualZeroSection D * segreClass'(L,D);
	chernClass' Thing := {} >> o -> i -> chernClass'(i,D);
	stableClass' (String,D) :=
	stableClass' (AryString,D) := {} >> o -> (i,D) -> (if curCotOpts.Ktheory then FF_0 else -1)^(2*dimvar-inversion i)*chernClass'(i,D);
	stableClass' (List,D) := {} >> o -> (L,D) -> (
	    q := if curCotOpts.Ktheory then FF_0 else -1;
	    chernClass'(L,D) * diagonalMatrix apply(L,i->q^(2*dimvar-inversion i))
	    );
	stableClass' Thing := {} >> o -> i -> stableClass'(i,D);
	schubertClass' (List,D) := {} >> o -> (L,D) -> ( -- should I cacheValue?
		inds := ind \ reverse \ L;
		map(M,M, apply(I,i->first entries (du fixedPoint(Rcheckz',reverse i))_inds))
		);
	schubertClass' (String,D) :=
	schubertClass' (AryString,D) := {} >> o -> (i,D) -> (
	    indi:=ind reverse i;
	    D apply(I,ii->(du fixedPoint(Rcheckz',reverse ii))_(0,indi))
	    );
	schubertClass' Thing := {} >> o -> i -> schubertClass'(i,D);
	--
	(D,FF,I)
    ) else error "Unknown presentation"
)

end

(M,FF,I)=setupCotangent(1,2,Ktheory=>true)
segreCls=segreClasses();
segreInv=segreCls^(-1);
Table table(I,I,(i,j)->segreInv*(segreClass i * segreClass j))
Table table(I,I,(i,j)->fugacityVector puzzle(i,j))
oo==ooo

(AA,BB,f,I) = setupCotangent(1,3,Ktheory=>true,Presentation=>Borel)
segreCls = segreClasses();
P=puzzle("011","101",Generic=>true,Equivariant=>true,Ktheory=>true)
(segreCls*fugacityVector P)_0 - segreClass(0,1,1)*segreClass(1,0,1)
