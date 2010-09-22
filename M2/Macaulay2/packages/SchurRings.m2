-- -*- coding: utf-8 -*-
--		Copyright 1996-2002,2004 by Daniel R. Grayson

newPackage(
	"SchurRings",
    	Version => "0.2", 
    	Date => "May 23, 2007",
    	Authors => {
	     {Name => "Michael Stillman", Email => "mike@math.cornell.edu", HomePage => "http://www.math.cornell.edu/~mike/"},
	     {Name => "Hal Schenck"}
	     },
    	Headline => "representation rings of general linear groups and of symmetric groups",
    	DebuggingMode => true
    	)

export {
     schurRing, 
     SchurRing, 
     symmRing, 
     toS, 
     toE, 
     toP, 
     toH, 
     plethysm, 
     jacobiTrudi, 
     Partitions,   -- ??
     zee,          -- rename
     symToChar,    -- ??
     charToSym,    -- ??
     scalarProd,   -- ??
     intProd,      -- ??
     cauchy,       -- need products of Schur rings
     wedge,        -- ??
     Stillman,     -- REMOVE!!
     Stembridge,   -- ??
     Schur,        -- what is this?
     Memoize,      -- ??
     EorH,         -- ??
     SchurRingIndexedVariableTable}
-- Improve the names/interface of the following:
--, symmRing, plethysmMap, jacobiTrudi, plethysm, cauchy, bott}

debug Core

SchurRing = new Type of EngineRing
SchurRing.synonym = "Schur ring"
monoid SchurRing := o -> R -> R.monoid
expression SchurRing := S -> new FunctionApplication from { schurRing, (S.Symbol, numgens monoid S) }
undocumented (expression, SchurRing)

toExternalString SchurRing := R -> toString expression R
undocumented (toExternalString, SchurRing),

toString SchurRing := R -> (
     if hasAttribute(R,ReverseDictionary) then toString getAttribute(R,ReverseDictionary)
     else toString expression R)
undocumented (toString, SchurRing)

net SchurRing := R -> (
     if hasAttribute(R,ReverseDictionary) then toString getAttribute(R,ReverseDictionary)
     else net expression R)
undocumented (net, SchurRing)

degreeLength SchurRing := (RM) -> degreeLength monoid RM
coefficientRing SchurRing := Ring => R -> last R.baseRings

ck := i -> if i < 0 then error "expected decreasing row lengths" else i

schur2monom = (a,M) -> (
     Mgens := M.generators;
     if # a === 0 then 1_M
     else product(# a, i -> (Mgens#i) ^ (
	       ck if i+1 < # a 
	       then a#i - a#(i+1)
	       else a#i)))

rawmonom2schur = (m) -> (
     t := new MutableHashTable;
     apply(rawSparseListFormMonomial m, (x,e) -> scan(0 .. x, i -> if t#?i then t#i = t#i + e else t#i = e)); 
     values t
     )

newSchur := (R,M,p) -> (
     if not (M.?Engine and M.Engine) 
     then error "expected ordered monoid handled by the engine";
     if not (R.?Engine and R.Engine) 
     then error "expected coefficient ring handled by the engine";
     RM := R M;
     SR := new SchurRing from rawSchurRing(RM.RawRing);
     SR.Symbol = p;
     SR.baseRings = append(R.baseRings,R);
     SR.basering = RM.basering;
     SR.FlatMonoid = M;
     SR.numallvars = numgens M;
     commonEngineRingInitializations SR;
     ONE := SR#1;
     if degreeLength M != 0 then (
	  -- there must be something smarter to do, but if we
	  -- do not do this, then we get into an infinite loop
	  -- because each monoid ring ZZ[a,b,c] needs its degrees ring
	  -- ZZ[t], which in turn needs to make its degrees ring 
	  -- ZZ[], which in turn needs one.
	  SR.degreesRing = degreesRing degreeLength M;
	  )
     else (
	  SR.degreesRing = ZZ;
	  );
     if R.?char then SR.char = R.char;
     SR.monoid = M;
     -- SR ? SR := (f,g) -> ( if f == g then symbol == else leadMonomial f ? leadMonomial g ); -- the engine should handle it.
     R * M := (r,m) -> new SR from rawTerm(SR.RawRing,raw r,m.RawMonomial);
     M * R := (m,r) -> new SR from rawTerm(SR.RawRing,raw r,m.RawMonomial);
     SR * M := (p,m) -> p * (R#1 * m);
     M * SR := (m,p) -> (R#1 * m) * p;
     R + M := (r,m) -> r * M#1 + R#1 * m;
     M + R := (m,r) -> r * M#1 + R#1 * m;
     SR + M := (p,m) -> p + R#1 * m;
     M + SR := (m,p) -> p + R#1 * m;
     R - M := (r,m) -> r * M#1 - R#1 * m;
     M - R := (m,r) -> R#1 * m - r * M#1;
     SR - M := (p,m) -> p - R#1 * m;
     M - SR := (m,p) -> R#1 * m - p;
     toExternalString SR := r -> toString expression r;
     expression SR := f -> (
	  (coeffs,monoms) -> sum(
	       coeffs,monoms,
	       (a,m) -> expression (if a == 1 then 1 else new R from a) *
	          new Subscript from {p, (
		    t1 := toSequence rawmonom2schur m;
		    if #t1 === 1 then t1#0 else t1
		    )})
	  ) rawPairs(raw R, raw f);
     listForm SR := (f) -> (
     	  n := numgens SR;
     	  (cc,mm) := rawPairs(raw R, raw f);
     	  toList apply(cc, mm, (c,m) -> (rawmonom2schur m, new R from c)));
     SR.generators = apply(M.generators, m -> SR#(toString m) = SR#0 + m);
     SR.use = x -> (
	  M + M := (m,n) -> R#1 * m + R#1 * n;
	  M - M := (m,n) -> R#1 * m - R#1 * n;
	  - M := (m,n) -> - R#1 * n;
	  scan(SR.baseRings, A -> (
	       if A =!= R then (
		    A * M := (i,m) -> (i * R#1) * m;
		    M * A := (m,i) -> m * (i * R#1);
		    );
	       A + M := (i,m) -> i * R#1 + m;
	       M + A := (m,i) -> m + i * R#1;
	       A - M := (i,m) -> i * R#1 - m;
	       M - A := (m,i) -> m - i * R#1;
	       M / A := (m,r) -> (m * ONE) / (r * ONE);
	       M % A := (m,r) -> (m * ONE) % (r * ONE);
	       ));
	  SR);
     -- leadMonomial R := f -> new M from rawLeadMonomial(n, f.RawRingElement); -- fix this?
     SR
     )

SchurRingIndexedVariableTable = new Type of IndexedVariableTable
SchurRingIndexedVariableTable _ Thing := (x,i) -> x#symbol _ i

schurRing = method (Options => {CoefficientRing => ZZ})
schurRing(Thing,ZZ) := SchurRing => opts -> (p,n) -> (
     try p = baseName p else error "schurRing: can't use provided thing as variable";
     if class p === Symbol then schurRing(p,n,opts)
     else error "schurRing: can't use provided thing as variable"
     );
schurRing(Symbol,ZZ) := SchurRing => opts -> (p,n) -> (
--     R := ZZ;
     R := opts.CoefficientRing;
     x := local x;
     prune := v -> drop(v, - # select(v,i -> i === 0));
     M := monoid[x_1 .. x_n];
     vec := apply(n, i -> apply(n, j -> if j<=i then 1 else 0));
     S := newSchur(R,M,p);
     dim S := s -> rawSchurDimension raw s;
     t := new SchurRingIndexedVariableTable from p;
     t.SchurRing = S;
     t#symbol _ = a -> ( m := schur2monom(a,M); new S from rawTerm(S.RawRing, raw (1_(coefficientRing S)), m.RawMonomial));
     S.use = S -> (globalAssign(p,t); S);
     S.use S;
     S)


SchurRing _ List := (S,a) -> (
     m := schur2monom(a,monoid S); 
     new S from rawTerm(S.RawRing, raw (1_(coefficientRing S)), m.RawMonomial)
     )

-- BUG in M2: R_0 .. R_n does not always give elements in the ring R!!
-- workaround:
varlist = (i,j,R) -> apply(i..j, p -> R_p)

--want SymmRing = new Type of PolynomialRing...

protect plethysmMaps
protect mapToE
protect Schur
protect mapToP

protect PtoETable
protect TtoPTable
protect HtoETable
protect HtoPTable
protect EtoPTable
protect PtoHTable
protect EtoHTable

protect symRingForE
protect symRingForP
protect mapFromE
protect mapFromP
protect grbE
protect grbH
protect grbP
protect mapSymToE
protect mapSymToP
protect mapSymToH

protect S
protect h
protect e
<< "WARNING or ERROR: change the names S,e,h to something else here!" << endl

symmRings := new MutableHashTable;
symmRing = (n) -> (
     if not symmRings#?n then (
     	  e := getSymbol "e";
     	  h := getSymbol "h";
     	  p := getSymbol "p";
     	  R := QQ[e_1..e_n,p_1..p_n,h_1..h_n,
	       Degrees => toList(1..n,1..n,1..n)];
     	  S := schurRing(getSymbol "s", n, CoefficientRing => QQ);
     	  R.Schur = S;
     	  R.dim = n;
	  
	  degsEHP := toList(1..n);
     	  blocks := {toList(0..(n-1)),toList(n..(2*n-1)),toList(2*n..(3*n-1))};
--     	  locVarsE := apply(blocks#0,i->R_i);
--     	  locVarsP := apply(blocks#1,i->R_i);
--     	  locVarsH := apply(blocks#2,i->R_i);
     	  vrs := symbol vrs;
     	  locVarsE := apply(blocks#0,i->vrs_i);
     	  locVarsP := apply(blocks#1,i->vrs_i);
     	  locVarsH := apply(blocks#2,i->vrs_i);
          R.symRingForE = QQ[locVarsH | locVarsP | locVarsE ,Degrees=>flatten toList(3:degsEHP),MonomialOrder=>GRevLex];
     	  R.mapToE = map(R.symRingForE,R,apply(blocks#2|blocks#1|blocks#0,i->(R.symRingForE)_i));
     	  R.mapFromE = map(R,R.symRingForE,apply(blocks#2|blocks#1|blocks#0,i->R_i));
     	  R.symRingForP = QQ[locVarsH | locVarsE | locVarsP,Degrees=>flatten toList(3:degsEHP),MonomialOrder=>GRevLex];
     	  R.mapToP = map(R.symRingForP,R,apply(blocks#1|blocks#2|blocks#0,i->(R.symRingForP)_i));
     	  R.mapFromP = map(R,R.symRingForP,apply(blocks#2|blocks#0|blocks#1,i->R_i));
     	  PtoE(n,R);
     	  HtoE(n,R);
     	  EtoH(n,R);
     	  PtoH(n,R);
     	  EtoP(n,R);
     	  HtoP(n,R);
     	  R.grbE = forceGB matrix{flatten apply(splice{1..n},i->{R.mapToE(R_(n-1+i))-R.PtoETable#i,R.mapToE(R_(2*n-1+i))-R.HtoETable#i})};
     	  R.grbH = forceGB matrix{flatten apply(splice{1..n},i->{R_(n-1+i)-R.PtoHTable#i,R_(-1+i)-R.EtoHTable#i})};
     	  R.grbP = forceGB matrix{flatten apply(splice{1..n},i->{R.mapToP(R_(-1+i))-R.EtoPTable#i,R.mapToP(R_(2*n-1+i))-R.HtoPTable#i})};
     	  collectGarbage();
     	  R.mapSymToE = (f) -> R.mapFromE(R.mapToE(f)%R.grbE);
     	  R.mapSymToP = (f) -> R.mapFromP(R.mapToP(f)%R.grbP);
     	  R.mapSymToH = (f) -> f%R.grbH;
--	  R.mapSymToE = map(R,R,flatten splice {varlist(0,n-1,R),apply(n, i -> PtoE(i+1,R)),apply(n, i -> HtoE(i+1,R))});
--     	  R.mapSymToP = map(R,R,flatten splice {apply(n, i -> EtoP(i+1,R)), varlist(n,2*n-1,R), apply(n, i -> HtoP(i+1,R))});
--     	  R.mapSymToH = map(R,R,flatten splice {apply(n, i -> EtoH(i+1,R)), apply(n, i -> PtoH(i+1,R)), varlist(2*n,3*n-1,R)});
     	  R.plethysmMaps = new MutableHashTable;
	  symmRings#n = R;
	  );
     symmRings#n)

---------------------------------------------------------------
--------------Jacobi-Trudi-------------------------------------
---------------------------------------------------------------

----local variables for jacobiTrudi
auxR = local auxR;
auxn = local auxn;
auxEH = local auxEH;
----

jacobiTrudi = method(Options => {Memoize => true, EorH => "H"})
jacobiTrudi(BasicList,Ring) := opts -> (lambda,R) ->
(
     lam := new Partition from lambda;
     rez := local rez;
     local u;
     if opts.EorH == "H" then u = h else (u = e;lam = conjugate lam;);
     if opts.Memoize then
     (
	  if not R.?S then R.S = new MutableHashTable;
	  if opts.EorH == "E" then
	  (
     	       -----S#0 records S-functions in terms of e
	       if not R.S#?0 then R.S#0 = new MutableHashTable;
	       auxEH = 0;
	       )
	  else
	  (
	       -----S#1 records S-functions in terms of h
	       if not R.S#?1 then R.S#1 = new MutableHashTable;
	       auxEH = 1;
	       );
     	  auxR = R;
     	  auxn = R.dim;
     	  rez = jT(lam);
	  )
     else
     (
     	  n := #lam;
     	  rez = det map(R^n, n, (i,j) -> 
	       (
	       	    aux := lam#i-i+j;
	       	    if aux < 0 then 0_R
	       	    else if aux == 0 then 1_R else u_aux)
	       )
	  );
     rez
     )

jT = (lambda) ->
(
     lambda = toList lambda;
     rez := local rez;
     if auxR.S#auxEH#?lambda then rez = auxR.S#auxEH#lambda
     else
     (
     ll := #lambda;
     if ll == 0 then rez = 1_auxR else
     if ll == 1 then rez = auxR_(2*auxEH*auxn-1+lambda#0) else
     (
	  l1 := drop(lambda,-1);
     	  l2 := {};
	  rez = 0;
	  sgn := 1;
	  for i from 0 to ll-1 do
	  (
     	       rez = rez + sgn*auxR_(2*auxEH*auxn-1+lambda#(ll-1-i)+i)*jT(l1|l2);
	       sgn = - sgn;
	       l1 = drop(l1,-1);
	       if lambda#(ll-1-i)>1 then
	       l2 = {lambda#(ll-1-i)-1} | l2;
	       );
	  );
     auxR.S#auxEH#lambda = rez;
     );
     rez
     )
---------------------------------------------------------------
--------------End Jacobi-Trudi---------------------------------
---------------------------------------------------------------


---------------------------------------------------------------
--------------Plethysm-----------------------------------------
---------------------------------------------------------------
plethysmMap = (d,R) -> (
     -- d is an integer
     -- R is symmRing n
     -- returns the map p_d : R --> R
     --    which sends p_i to p_(i*d).
     n := R.dim;
     if not R.plethysmMaps#?d then (
	 fs := splice {n:0_R,apply(1..(n//d), j -> R_(n-1+d*j)),(2*n-n//d):0_R};
         R.plethysmMaps#d = map(R,R,fs);
	 );
     R.plethysmMaps#d
     )

plethysm = method()
plethysm(RingElement,RingElement) := (f,g) -> (
     -- f is a polynomial in symmRing N / SchurRing SA
     -- g is a polynomial in symmRing n / SchurRing SB
     -- result is in symmRing n / SchurRing SB
     R := ring g;
     Rf := ring f;
     issy := true;
     df := local df;
     dg := local dg;
     
     if class Rf === SchurRing then
     (
	  df = degSchurPol(f);
	  Rf = symmRing df;
	  f = toP(f,Rf);
	  )
     else 
     (
	  df = (degree f)#0;
	  f = toP f;
	  );
     
     if class R === SchurRing then
     (
	  issy = false;
	  SB := R;
	  dg = degSchurPol(g);
	  d := df*dg;
	  R = symmRing d;
	  g = toP(g,R);
	  )
     else 
     (
	  dg = (degree g)#0;
	  g = toP g;
	  );

     n := R.dim;
     if n<df*dg then error"Need symmetric ring of higher dimension";

     N := Rf.dim;
     phi := map(R,Rf,flatten splice {N:0_R,apply(1..N, j -> (plethysmMap(j,R)) g),N:0_R});
     if issy then phi f
     else toS(phi f,SB))

plethysm(BasicList,RingElement) := (lambda,g) -> (
     d := sum toList lambda;
     Rf := symmRing d;
     f := jacobiTrudi(lambda,Rf);
     plethysm(f,g))

--degree of a polynomial in a SchurRing
degSchurPol = method()
degSchurPol(RingElement) := ps -> (
     (mon,coe) := coefficients ps;
     parmon := apply(flatten entries mon,i -> value toString last i);
     max apply(parmon,i->sum i)
     )
---------------------------------------------------------------
-----------End plethysm----------------------------------------
---------------------------------------------------------------


---------------------------------------------------------------
----Transition between various types of symmetric functions----
---------------------------------------------------------------

toSymm = method()
--ps should be an element of a schurRing, R a symmRing
--toSymm returns the symmetric function corresponding to ps
toSymm(RingElement,Ring) := (ps,R) ->
(
     S := ring ps;
     (mon,coe) := coefficients ps;
     parmon := apply(flatten entries mon,i -> value toString last i);
     --deg = max apply(parmon,i->sum i);
     --R = symmRing deg;
     liftcoe := apply(flatten entries coe,i->lift(i,coefficientRing S));
     symmon := apply(parmon,i->(try a:=jacobiTrudi(i,R) then a else error"Need symmetric ring of higher dimension"));
     sum for i from 0 to #liftcoe-1 list liftcoe#i*symmon#i
)

toE = method()
--writes a symmetric function in terms of
--elementary symmetric polynomials
toE (RingElement) := (f) -> (ring f).mapSymToE f
--f should be an element of a symmRing
toE (RingElement,Ring) := (ps,R) ->
--ps should be an element of a schurRing, R a symmRing
(
     toE toSymm(ps,R)
     )

toP = method()
--writes a symmetric function in terms of
--power sums
toP (RingElement) := (f) -> (ring f).mapSymToP f
--f should be an element of a symmRing
toP (RingElement,Ring) := (ps,R) ->
--ps should be an element of a schurRing, R a symmRing
(
     toP toSymm(ps,R)
     )

toH = method()
--writes a symmetric function in terms of
--complete symmetric polynomials
toH (RingElement) := (f) -> (ring f).mapSymToH f
--f should be an element of a symmRing
toH (RingElement,Ring) := (ps,R) ->
--ps should be an element of a schurRing, R a symmRing
(
     toH toSymm(ps,R)
     )

toS = method(Options => {Strategy => Stembridge, Memoize => true})
toS(RingElement,SchurRing) := opts -> (f,S) -> (
     -- f is a polynomial in 'symmRing n', of degree d<=n
     -- S is a SchurRing
     local hf;
     local mtos;
     R := ring f;
     n := R.dim;
     d := first degree f;
     if d>n then error"need symmetric ring of higher dimension";
     rez := 0_S;
     s := S.Symbol;
     ngS := numgens S;
     use S;
     if opts.Strategy == Stembridge then
     (
     	  hf = toH(f);
     	  while (hf!=0) do
     	  (
     	       lt := leadTerm hf;
     	       (mon,coe) := coefficients lt;
     	       degs := (flatten exponents mon_0_0)_{(2*n)..(3*n-1)};
     	       par := {};
     	       for i from 0 to n-1 do
	           par = par | splice{degs#i:(i+1)};
	       par = reverse par;
	       hf = hf - coe_0_0*(jacobiTrudi(par,R,Memoize => opts.Memoize));
	       if #par <= ngS then
	          rez = rez + lift(coe_0_0,coefficientRing S)*value(toString s_par);--value toString is kind of stupid..
     	       );
     )
     else if opts.Strategy == Stillman then
     (
	  hf = toH(f);
	  ef := toE(f);
	  le := #terms(ef);
	  lh := #terms(hf);
	  if le<lh then 
	  (
	       mtos = map(S,R,apply(splice{1..n},i->(if i<=ngS then value(toString s_(splice{i:1})) else 0)) | splice{2*n:0});
     	       rez = mtos(ef);
	       )
	  else 	  
	  (
	       mtos = map(S,R,splice{2*n:0} | apply(splice{1..n},i->(if i<=ngS then value(toString s_{i}) else 0)));
     	       rez = mtos(hf);
	       );
	  )
     else error"Invalid strategy";
     rez
     )
toS(RingElement) := opts -> (f) -> (toS(f,(ring f).Schur,opts))

PtoE = (m,R) -> (
     -- R is a symmring n
     -- R should have a field named PtoETable, which is
     --  a mutable hash table with i => p_i values for i = 1,...,??
     -- this computes the values up through m.
     n := R.dim;
     if not R.?PtoETable then
     	  R.PtoETable = new MutableHashTable;
     PE := R.PtoETable;
     s := #(keys PE);
--     if (s<m) then
--     (
     for i from s+1 to m do (
	  f := if i > n then 0 else -i*(R.symRingForE)_(2*n+i-1); -- R_(i-1) IS e_i
	  for r from max(1,i-n) to i-1 do 
	       f = f + (-1)^(r-1) * (R.symRingForE)_(2*n+i-r-1) * PE#r; -- R_(i-r-1) IS e_(i-r)
	  PE#i = if i%2 == 0 then f else -f;
	  );
--     initSymR(R);
--     R.grbPE = forceGB matrix{flatten apply(splice{1..m},i->{R.mapToE(R_(n-1+i)-R.PtoETable#i)})};
--     collectGarbage();
--     );
     PE#m
     )

HtoE = (m,R) -> (
     -- R is a symmring n
     -- R should have a field named HtoETable, which is
     --  a mutable hash table with i => e_i values for i = 1,...,??
     -- this computes the values up through m.
     n := R.dim;
     if not R.?HtoETable then
     	  R.HtoETable = new MutableHashTable;
     HE := R.HtoETable;
     s := #(keys HE);
--     if (s<m) then
--     (
     for i from s+1 to m do (
	  f := if i > n then 0 else (-1)^(i-1)*(R.symRingForE)_(2*n+i-1); -- R_(i-1) IS e_i
	  for r from 1 to min(i-1,n-1) do 
	       f = f + (-1)^(r-1) * (R.symRingForE)_(2*n+r-1) * HE#(i-r); -- R_(r-1) IS e_r
	  HE#i = f;
	  );
--     initSymR(R);
--     R.grbHE = forceGB matrix{flatten apply(splice{1..m},i->{R.mapToE(R_(2*n-1+i)-R.HtoETable#i)})};
--     collectGarbage();
--     );
     HE#m
     )

EtoH = (m,R) -> (
     -- R is a symmring n
     -- R should have a field named EtoHTable, which is
     --  a mutable hash table with i => h_i values for i = 1,...,??
     -- this computes the values up through m.
     n := R.dim;
     if not R.?EtoHTable then
     	  R.EtoHTable = new MutableHashTable;
     EH := R.EtoHTable;
     s := #(keys EH);
     if m > n then return 0_R;
--     if (s<m) then
--     (
     for i from s+1 to m do (
	  f := R_(2*n-1+i); -- R_(2*n-1+i) IS h_i
	  for r from 1 to i-1 do 
	       f = f + (-1)^r * EH#r * R_(2*n-1+i-r); -- R_(2*n-1+i-r) IS h_(i-r)
	  EH#i = if i%2 == 1 then f else -f;
	  );
--     initSymR(R);
--     R.grbEH = forceGB matrix{flatten apply(splice{1..m},i->{R_(-1+i)-R.EtoHTable#i})};
--     collectGarbage();
--     );
     EH#m
     )

EtoP = (m,R) -> (
     -- R is a symmring n
     -- R should have a field named EtoPTable, which is
     --  a mutable hash table with i => p_i for i = 1,...,??
     -- this computes the values up through m.
     n := R.dim;
     if not R.?EtoPTable then (
     	  R.EtoPTable = new MutableHashTable;
	  R.EtoPTable#0 = 1;
	  );
     EP := R.EtoPTable;
     s := #(keys EP); -- keys includes 0,1,2,...
     if m > n then return 0_R;
--     if (s<=m) then
--     (
     for i from s to m do (
	  f := 0;
	  for r from 1 to i do 
	       f = f + (-1)^(r-1) * EP#(i-r) * (R.symRingForP)_(2*n-1+r); -- R_(n-1+r) IS p_r
	  EP#i = (1/i) * f;
	  );
--     initSymR(R);
--     R.grbEP = forceGB matrix{flatten apply(splice{1..m},i->{R.mapToP(R_(-1+i)-R.EtoPTable#i)})};
--     collectGarbage();
--     );
     EP#m
     )

HtoP = (m,R) -> (
     -- R is a symmring n
     -- R should have a field named HtoPTable, which is
     --  a mutable hash table with i => h_i for i = 1,...,??
     -- this computes the values up through m.
     n := R.dim;
     if not R.?HtoPTable then (
     	  R.HtoPTable = new MutableHashTable;
	  R.HtoPTable#0 = 1;
	  );
     HP := R.HtoPTable;
     s := #(keys HP); -- keys includes 0,1,2,...
     if m>n then error("need symmetric ring of higher dimension");
--     if (s<=m) then
--     (
     for i from s to m do (
	  f := 0;
	  for r from 1 to i do 
	       f = f + HP#(i-r) * (R.symRingForP)_(2*n-1+r); -- R_(n-1+r) IS p_r
	  HP#i = (1/i) * f;
	  );
--     initSymR(R);
--     R.grbHP = forceGB matrix{flatten apply(splice{1..m},i->{R.mapToP(R_(2*n-1+i)-R.HtoPTable#i)})};
--     collectGarbage();
--     );
     HP#m
     )

PtoH = (m,R) -> (
     -- R is a symmring n
     -- R should have a field named PtoHTable, which is
     --  a mutable hash table with i => p_i for i = 1,...,??
     -- this computes the values up through m.
     n := R.dim;
     if not R.?PtoHTable then (
     	  R.PtoHTable = new MutableHashTable;
	  R.PtoHTable#0 = 1;
	  );
     PH := R.PtoHTable;
     s := #(keys PH); -- keys includes 0,1,2,...
     if m>n then error("need symmetric ring of higher dimension");
--     if (s<=m) then
--     (
     for i from s to m do (
	  f := i*R_(2*n-1+i);  -- R_(2*n-1+i) IS h_i
	  for r from 1 to i-1 do 
	       f = f - PH#r * R_(2*n-1+i-r); -- R_(2*n-1+i-r) IS h_(i-r)
	  PH#i = f;
	  );
--     initSymR(R);
--     R.grbPH = forceGB matrix{flatten apply(splice{1..m},i->{R_(n-1+i)-R.PtoHTable#i})};
--     collectGarbage();
--     );
     PH#m
     )
---------------------------------------------------------------
--------------End transition-----------------------------------
---------------------------------------------------------------

---------------------------------------------------------------
--------------Characters of Symmetric Group--------------------
---------------------------------------------------------------

seqToMults = method()
seqToMults(List) := (lambda) ->
(
     lam := new Partition from lambda;
     aux := toList(conjugate lam)|{0};
     rez := {};
     for j from 0 to #aux-2 do
     (
     	  dif := aux#j-aux#(j+1);
       	  rez = rez | {dif};
	  );
     rez 
     )

multsToSeq = method()
multsToSeq(List) := (mults) ->
(
     n := #mults;
     par := {};
     for i from 0 to n-1 do
         par = par | splice{mults#i:(i+1)};
     reverse par
     )

zee = method()
zee(List) := lambda ->
(
     product for i from 0 to #lambda-1 list((i+1)^(lambda#i)*(lambda#i)!)
     )

symToChar = method()
symToChar(RingElement) := (f)->
(
     R := local R;
     pf := local pf;
     Rf := ring f;
     if class Rf === SchurRing then
     (
     	  R = symmRing degSchurPol(f);
	  pf = toP(f,R);
	  )
     else
     (
	  R = Rf;
     	  pf = toP f;
	  );
     n := R.dim;
     (mon,coe) := apply(coefficients pf,i->flatten entries i);
--     exps := apply(exponents pf,i->i_{n..(2*n-1)});
     ch := new MutableHashTable;
     for j from 0 to #mon-1 do
     (
     	  degs := (flatten exponents mon#j)_{(n)..(2*n-1)};
     	  par := multsToSeq(degs);
	  ch#par = lift(coe#j,coefficientRing R) * zee(degs);
	  );
     new HashTable from ch
     )

charToSym = method()
charToSym(HashTable,Ring) := (ch,R)->
(
     rez := 0_R;
     n := R.dim;
     for lam in keys ch do
     	  rez = rez + ch#lam * (product for i from 0 to #lam-1 list R_(n-1+lam#i)) / zee(seqToMults lam);
     rez
     )

scalarProd = method()
scalarProd(HashTable,HashTable) := (ch1,ch2)->
(
     scProd := 0;
     chProd := intProd(ch1,ch2);
     for i in keys(chProd) do
     	  scProd = scProd + chProd#i / zee(seqToMults i);
     scProd
     )

scalarProd(RingElement,RingElement) := (f1,f2)->
(
     ch1 := symToChar f1;
     ch2 := symToChar f2;
     scalarProd(ch1,ch2)
     )

intProd = method()
intProd(HashTable,HashTable) := (ch1,ch2)->
(
     iProd := new MutableHashTable;
     l1 := sum((keys ch1)#0);
     l2 := sum((keys ch2)#0);
     if l1 != l2 then error("The symmetric functions/characters must have the same degree");
     for i in keys(ch1) do
     	  if ch2#?i then iProd#i = ch1#i * ch2#i;
     new HashTable from iProd
     )

intProd(RingElement,RingElement) := (f1,f2)->
(
     R2 := ring f2;
     R := local R;
     issy := false;
     if (class R2 =!= SchurRing) then (R = R2; issy = true;)
     else R = symmRing degSchurPol(f2);
     ch1 := symToChar f1;
     ch2 := symToChar f2;
     rez := charToSym(intProd(ch1,ch2),R);
     if issy then rez else
     toS(rez,R2)
     )

chi(BasicList,BasicList) := (lambda, rho) ->
(
     la := toList lambda;
     rh := toList rho;
     ll := sum la;
     if ll != sum(rh) then error"Partitions must have the same size.";
     R := symmRing ll;
     sl := jacobiTrudi(la,R);
     pr := 1_R;
     for i from 0 to #rh-1 do pr = pr * R_(ll-1+rh#i);
     scalarProd(sl,pr)
     )

---------------------------------------------------------------
--------------End characters-----------------------------------
---------------------------------------------------------------

---------------------------------------------------------------
-----------Partitions-related functions------------------------
---------------------------------------------------------------
parts := (d, n) -> (
     -- d is an integer >= 0
     -- n is an integer >= 1
     -- returns a list of all of the partitions of d
     --    having <= n parts.
     x := partitions(d);
     select(x, xi -> #xi <= n))     

-------Generate all the partitions of a set
-------with a given shape
locS = local locS;
locL = local locL;
locLengthL = local locLengthL;
locParts = local locParts;
locPartitions = local locPartitions;
genPartitions = local genPartitions;

genPartitions = method()
genPartitions(ZZ) := (k)->
(
     if k==length locS then (locPartitions = locPartitions | {set toList locParts}) else
     (
     for i from 0 to locLengthL-1 do
     	  if (i==0 and #locParts#0 < locL#0) or (((locL#(i-1)>locL#i) or (#locParts#(i-1)>0)) and (#locParts#i<locL#i)) then
	  (
	       locParts#i = locParts#i + set{locS#k};
	       genPartitions(k+1);
	       locParts#i = locParts#i - set{locS#k};
	       );
     )
);

Partitions = method()
Partitions(Set,BasicList) := (S,L)->
(
     locS = toList S;
     locL = L;
     locLengthL = #L;
     locParts = new MutableList;
     for i from 0 to locLengthL-1 do locParts#i = set{};
     locPartitions = {};
     genPartitions(0);
     locPartitions
     )

--------end generate partitions

---------------------------------------------------------------
--------End partitions-related functions-----------------------
---------------------------------------------------------------


---------------------------------------------------------------
--------Old stuff----------------------------------------------
---------------------------------------------------------------
{*
restart
loadPackage"SchurRings"
----wedge powers over GL(V) x GL(W)
S = schurRing(a,5)
T = schurRing(b,5)
cauchy(3,a_{1},b_{1})
wedge(2,{(a_{1},b_{1}),(a_{3}+a_{2,1},b_{2,2})})
wedge(2,{(a_{1},b_{1}),(a_{1},b_{1})})

r = 3
L = {(a_{1},b_{1}), (a_{3}+a_{2,1},b_{2,2})}

----end wedge powers
*}

cauchy = (i,f,g) -> (
     -- f and g are elements of Schur rings (possibly different)
     -- compute the i th exterior power of the representation f ** g
     P := partitions i;
     result := apply(P, lambda -> (
	       (
		   a := plethysm(lambda,f);
		   if a == 0 then null
		   else (
			b := plethysm(conjugate lambda, g);
			if b == 0 then null else (a,b)
		    ))));
     select(result, x -> x =!= null)
     )

compositions1 = (r,n) -> (
     -- return a list of all of the n-compositions of r.
     -- i.e. ways to write r as a sum of n nonnegative integers
     if n === 1 then {{r}}
     else if r === 0 then {toList(n:0)}
     else (
	  flatten for i from 0 to r list (
	       w := compositions1(r-i,n-1);
	       apply(w, w1 -> prepend(i,w1)))))


pairProduct = L -> (
     -- L is a list of lists of (f,g), f,g both in Schur/symmetric rings.
     -- result: a list of pairs (f,g).
     if #L === 1 then first L
     else (
	  L' := drop(L,1);
	  P' := pairProduct L';
	  flatten apply(L#0, fg -> (
	       (f,g) := fg;
	       apply(P', pq -> (
		    (p,q) := pq;
		    (f*p, g*q)))))
     ))
----e.g. L = {{(h_2,e_3)},{(h_1,e_3),(p_2,e_2)}}
----pairProduct L = {(h_1*h_2,e_3^2), (p_2*h_2,e_2*e_3)}

wedge = method()		    
wedge(List,List) := (C,L) -> (
     -- C is a composition of 0..n-1, n == #L
     -- form the product of the exterior powers of the corresponding representations.
     result := {}; -- each entry will be of the form (f,g)
     C0 := positions(C, x -> x =!= 0);
     wedgeL := apply(C0, i -> cauchy(C#i,L#i#0,L#i#1));
     pairProduct wedgeL
     )

wedge(ZZ,List) := (r,L) -> (
     -- r is an integer >= 1
     -- L is a list of pairs (f,g), f,g are in (possibly different) Schur rings.
     -- returns wedge(r)(L), as a sum of representations of GL(m) x GL(n)
     n := #L;
     p := compositions1(r,n);
     flatten apply(p, x -> wedge(x,L))
     )
--this computes the r-th wedge power of the direct sum of
--V_i\tensor W_i where V_i, W_i are GL(V) and GL(W) (virtual) modules
--corresponding to pairs of (virtual) characters (f_i,g_i)


---------------------------------------------------------------
--------End old stuff----------------------------------------------
---------------------------------------------------------------

beginDocumentation()

undocumented (wedge,List,List)
undocumented {EorH, Schur}

document {
     Key => "SchurRings",
     Headline => "rings representing irreducible representations of GL(n)",
     "This package makes computations in the representation rings of general linear groups and symmetric groups possible.",
     PARA{},
     "Given a positive integer ", TT "n", ", 
     we may define a polynomial ring over ", TO "ZZ", " (or ", TO "QQ", ") in ", TT "n", " variables, whose
     monomials correspond to the irreducible representations of GL(n), and where 
     multiplication is given by the decomposition of the tensor product of representations",
     PARA{},
     "We create such a ring in Macaulay2 using the ", TO schurRing, " function:",
     EXAMPLE "R = schurRing(s,4);",
     "A monomial represents the irreducible representation with a given highest weight. 
     The standard 4 dimensional representation is",
     EXAMPLE "V = s_{1}",
     "We may see the dimension of the corresponding irreducible representation using ", TO "dim",
     ":",
     EXAMPLE "dim V",
     "The third symmetric power of V is obtained by",
     EXAMPLE {
	  "W = s_{3}",
     	  "dim W"},
     "and the third exterior power of V can be obtained using",
     EXAMPLE {
	  "U = s_{1,1,1}",
	  "dim U"},
     "Multiplication of elements corresponds to tensor product of representations.  The 
     value is computed using a variant of the Littlewood-Richardson rule.",
     EXAMPLE {
	  "V * V",
	  "V^3"},
     "One cannot make quotients of this ring, and Groebner bases and related computations
     do not work, but I'm not sure what they would mean..."
     }

document {
     Key => {schurRing,(schurRing,Symbol,ZZ),(schurRing,Thing,ZZ)},
     Headline => "Make a Schur ring",
     TT "schurRing(s,n)", " -- creates a Schur ring of degree n with variables based on the symbol s",
     PARA{"This is the representation ring for the general linear group of ", TT "n", " by ", TT "n", " matrices."},
     PARA{"If ", TT "s", " is already assigned a values as a variable in a ring, its base symbol will be used,
	  if it is possible to determine."},
     SeeAlso => {"SchurRing", "symmRing"}}

document {
     Key => {SchurRing, (degreeLength,SchurRing), (coefficientRing, SchurRing), (monoid, SchurRing)},
     Headline => "the class of all Schur rings",
     "A Schur ring is the representation ring for the general linear group of 
     n by n matrices, and one can be constructed with ", TO schurRing, ".",
     EXAMPLE "R = schurRing(s, 4)",
     "The element corresponding to the Young diagram ", TT "{3,2,1}", " is
     obtained as follows.",
     EXAMPLE "s_{3,2,1}",
     "The dimension of the underlying virtual representation can be obtained
     with ", TO "dim", ".",
     EXAMPLE "dim s_{3,2,1}",
     "Multiplication in the ring comes from tensor product of representations.",
     EXAMPLE "s_{3,2,1} * s_{1,1}",
     SeeAlso => {schurRing}}

doc ///
Key
  [schurRing,CoefficientRing]
Headline
  The coefficient ring of a Schur ring
Usage
  CoefficientRing => R
Inputs
  R:Ring
Description
  Text
    
    This option allows one to choose the base ring for a Schur ring.
  
Caveat
  This only works when {\tt R} is @TO ZZ@ or @TO QQ@. One would
  like to have Schur rings over arbitrary base rings.
///
-- document {
--      Key => (symbol _, SchurRing, List),
--      Headline => "make an element of a Schur ring",
--      TT "S_v", " -- produce the element of the Schur ring ", TT "S", " corresponding
--      to the Young diagram whose rows have lengths as in the list ", TT "v", ".",
--      PARA{},
--      "The row lengths should be in decreasing order.",
--      SeeAlso => "SchurRing"}

document {
     Key => {SchurRingIndexedVariableTable,(symbol _,SchurRingIndexedVariableTable,Thing)},
     "This class is used as part of the implementation of a type of indexed variable used just for Schur rings.",
     SeeAlso => { IndexedVariableTable }
     }

doc ///
Key
  symmRing
Headline
  Make a Symmetric ring
Usage
  symmRing n
Inputs
  n:ZZ
Description
  Text

    {\tt symmRing n} creates a Symmetric ring of dimension {\tt n}.
    This is the subring of the ring of symmetric functions consisting
    of polynomials in the first {\tt n} elementary (or complete, or power sum)
    symmetric functions.

SeeAlso
  SchurRing
///

doc ///
   Key
     toS
   Headline 
     Schur (s-) basis representation
   Description
      Text
      
     	  Given a symmetric function {\tt f}, the function 
	  {\tt toS} yields a representation of {\tt f} as a linear
	  combination of Schur functions. 
      
      Example
      	  R = symmRing 6;
	  toS(h_3*e_3)
      
      Text	  
	
	  If a {\tt SchurRing S} is provided as input, then one interprets
	  the input function {\tt f} as a (virtual) character of a certain general linear
	  group, and the output as a representation of {\tt f} as a sum of characters.
      
      Example
      	  R = symmRing 6;
	  S = schurRing(s,3);
	  toS(h_3*e_3,S)
      
      Text
      
      	  One can notice that the outputs of two examples above 
	  do not coincide. This is because the former example gives
	  an actual symmetric function, while the latter gives an 
	  element of the representation ring of GL_3, which is a quotient
	  of the ring of symmetric functions.
   SeeAlso
     toH
     toE
     toP
///

doc ///
   Key
     (toS,RingElement)
   Headline
     Represents a symmetric function in the s-basis
   Usage
     fs = toS f
   Inputs
     f:RingElement
       element of a Symmetric ring
   Outputs
     fs:RingElement
        element of a Schur ring
   Description
      Text

        The input function {\tt f} should be interpreted as a symmetric
	function, as well as the output, which is an element of the Schur
	ring {\tt R.Schur} attached to the Symmetric ring {\tt R} of {\tt f}.

      Example
        R = symmRing 4;
        toS(e_1*h_2+p_3)

      Text

        An error is returned if the degree of the input function {\tt f}
	is larger than the dimension of the Symmetric ring of {\tt f}.
///

doc ///
   Key
     (toS,RingElement,SchurRing)
   Headline
     Represents a virtual character in the s-basis
   Usage
     fs = toS(f,S)
   Inputs
     f:RingElement
       element of a Symmetric ring
     S:SchurRing
   Outputs
     fs:RingElement
        element of {\tt S}
   Description
      Text

        The input function {\tt f} should be interpreted as a virtual character
	as well as the output, which is an element of {\tt S}, the representation
	ring of a certain general linear group.

      Example
        R = symmRing 8;
	S = schurRing(s,2);
        toS(h_2^4,S)

      Text

        An error is returned if the degree of the input function {\tt f}
	is larger than the dimension of the Symmetric ring of {\tt f}.
///

doc///
   Key
     [toS,Memoize]
   Headline
     Store values of the jacobiTrudi function.
   Usage
     Memoize => b
   Inputs
     b:Boolean
   Description
     Text

       This option is relevant when the method {\tt toS} is used with the
       default @TO Stembridge@ strategy. If the option is set to {\tt true}
       then all the values of the @TO jacobiTrudi@ function that are computed in
       the process are recorded into a special hash table attached to the
       symmetric ring inside which the computations are done.
///   

doc///
   Key
    [toS,Strategy]
   Headline
     Stembridge or Stillman's strategy.
   Description
     Text
       The @TO Stembridge@ strategy refers to the method of computing the
       representation of a symmetric function in the s-basis used by John
       Stembridge in his Maple SF package. The bottleneck of this strategy
       is the computation of Jacobi-Trudi determinants, so this is best used
       with the option {\tt Memoize} set to {\tt true}.
       
       If the amount of available memory is an issue, then one can use the
       @TO Stillman@ strategy, in which case the option {\tt Memoize} is
       irrelevant.
///   

doc ///
  Key
    toE
  Headline
     Elementary symmetric (e-) basis representation
  Description
      Text

          Given a symmetric function {\tt f}, the function 
          {\tt toE} yields a representation of {\tt f} as a polynomial
	  in the elementary symmetric functions.

      Example
      	  R = symmRing 7;
	  toE(h_3*e_3+p_2*h_5+h_1^2*p_3)
      
      Text	  
	
	  The symmetric function {\tt f} need not be an element of a Symmetric
	  ring. It may be an element of a Schur ring, in which case one needs
	  to specify a Symmetric ring to carry the e-representation of {\tt f}.
      
      Example
      	  R = symmRing 9;
	  S = schurRing(s,3);
	  toE(s_{3,2,1}*s_{2}+s_{4,2},R)
  SeeAlso
    toH
    toS
    toP
///

doc ///
   Key
     (toE,RingElement)
   Headline
     Represents a symmetric function in the e-basis
   Usage
     fe = toE f
   Inputs
     f:RingElement
       element of a Symmetric ring
   Outputs
     fe:RingElement
        element of a Symmetric ring
   Description
      Text

        If the input {\tt f} is a symmetric function, element of a 
	Symmetric ring {\tt R}, then the output {\tt fe} is
	the representation of {\tt f} as a polynomial in the 
	elementary symmetric functions in {\tt R}.

      Example
        R = symmRing 4;
        toE(e_1*h_2+p_3)
///

doc ///
   Key
     (toE,RingElement,Ring)
   Headline
     Represents a symmetric function in the e-basis
   Usage
     fe = toE(f,R)
   Inputs
     f:RingElement
       element of a Schur ring
     R:Ring
       a Symmetric ring
   Outputs
     fe:RingElement
        element of a Symmetric ring
   Description
      Text

        If {\tt f} is a symmetric function, represented as an element
	of a Schur ring, and {\tt R} is a Symmetric ring, then the output {\tt fe}
	is the representation of {\tt f} as a polynomial in the 
	elementary symmetric functions in {\tt R}.

      Example
        R = symmRing 7;
	S = schurRing(s,3);
        toE(s_{3,2,1}*s_{2}+s_{2,1}^2,R)

      Text

        An error is returned if the representation of {\tt f}
	involves symmetric functions {\tt e_n} for {\tt n} larger
	than the dimension of {\tt R}.
///

doc ///
  Key
    toH
  Headline
     Complete symmetric (h-) basis representation
  Description
      Text

          Given a symmetric function {\tt f}, the function 
          {\tt toH} yields a representation of {\tt f} as a polynomial
	  in the complete symmetric functions.

      Example
      	  R = symmRing 7;
	  toH(h_3*e_3+p_2*h_5+h_1^2*p_3)
      
      Text	  
	
	  The symmetric function {\tt f} need not be an element of a Symmetric
	  ring. It may be an element of a Schur ring, in which case one needs
	  to specify a Symmetric ring to carry the h-representation of {\tt f}.
      
      Example
      	  R = symmRing 9;
	  S = schurRing(s,3);
	  toH(s_{3,2,1}*s_{2}+s_{4,2},R)
  SeeAlso
    toE
    toS
    toP
///

doc ///
   Key
     (toH,RingElement)
   Headline
     Represents a symmetric function in the h-basis
   Usage
     fh = toH f
   Inputs
     f:RingElement
       element of a Symmetric ring
   Outputs
     fh:RingElement
        element of a Symmetric ring
   Description
      Text

        If the input {\tt f} is a symmetric function, element of a 
	Symmetric ring {\tt R}, then the output {\tt fh} is
	the representation of {\tt f} as a polynomial in the 
	complete symmetric functions in {\tt R}.

      Example
        R = symmRing 4;
        toH(e_1*h_2+p_3)
///

doc ///
   Key
     (toH,RingElement,Ring)
   Headline
     Represents a symmetric function in the h-basis
   Usage
     fh = toH(f,R)
   Inputs
     f:RingElement
       element of a Schur ring
     R:Ring
       a Symmetric ring
   Outputs
     fh:RingElement
        element of a Symmetric ring
   Description
      Text

        If {\tt f} is a symmetric function, represented as an element
	of a Schur ring, and {\tt R} is a Symmetric ring, then the output {\tt fh}
	is the representation of {\tt f} as a polynomial in the 
	complete symmetric functions in {\tt R}.

      Example
        R = symmRing 7;
	S = schurRing(s,3);
        toH(s_{3,2,1}*s_{2}+s_{2,1}^2,R)

      Text

        An error is returned if the representation of {\tt f}
	involves symmetric functions {\tt h_n} for {\tt n} larger
	than the dimension of {\tt R}.
///

doc ///
  Key
    toP
  Headline
     Power-sum (p-) basis representation
  Description
      Text

          Given a symmetric function {\tt f}, the function 
          {\tt toP} yields a representation of {\tt f} as a polynomial
	  in the power-sum symmetric functions.

      Example
      	  R = symmRing 7;
	  toP(2*(e_1^2-e_2))

      Text	  
	
	  The symmetric function {\tt f} need not be an element of a Symmetric
	  ring. It may be an element of a Schur ring, in which case one needs
	  to specify a Symmetric ring to carry the p-representation of {\tt f}.
      
      Example
      	  R = symmRing 6;
	  S = schurRing(s,2);
	  toP(2*s_{3}-s_{2,1},R)
  SeeAlso
    toH
    toS
    toE
///

doc ///
   Key
     (toP,RingElement)
   Headline
     Represents a symmetric function in the p-basis
   Usage
     fp = toP f
   Inputs
     f:RingElement
       element of a Symmetric ring
   Outputs
     fp:RingElement
        element of a Symmetric ring
   Description
      Text

        If the input {\tt f} is a symmetric function, element of a 
	Symmetric ring {\tt R}, then the output {\tt fp} is
	the representation of {\tt f} as a polynomial in the 
	power-sum symmetric functions in {\tt R}.

      Example
        R = symmRing 4;
        toP(e_1*h_2+p_3)
///

doc ///
   Key
     (toP,RingElement,Ring)
   Headline
     Represents a symmetric function in the p-basis
   Usage
     fp = toP(f,R)
   Inputs
     f:RingElement
       element of a Schur ring
     R:Ring
       a Symmetric ring
   Outputs
     fp:RingElement
        element of a Symmetric ring
   Description
      Text

        If {\tt f} is a symmetric function, represented as an element
	of a Schur ring, and {\tt R} is a Symmetric ring, then the output {\tt fp}
	is the representation of {\tt f} as a polynomial in the 
	power-sum symmetric functions in {\tt R}.

      Example
        R = symmRing 5;
        S = schurRing(s,3);
        toP(s_{2,1}*s_{2},R)

      Text

        An error is returned if the representation of {\tt f}
	involves symmetric functions {\tt p_n} for {\tt n} larger
	than the dimension of {\tt R}.
///

doc ///
Key
  jacobiTrudi
  (jacobiTrudi,BasicList,Ring)
Headline
  Jacobi-Trudi determinant
Usage
  f = jacobiTrudi(lambda,R)
Inputs
  lambda:BasicList
         a nonincreasing list of integers, or a partition
  R:Ring
    a Symmetric ring
Outputs
  f:RingElement
    an element of a Symmetric ring
Description
  Text
  
    Given a partition {\tt lambda} and Symmetric ring {\tt R},
    the method evaluates the Jacobi-Trudi determinant corresponding
    to the partition {\tt lambda}, yielding a representation of
    the Schur function {\tt s_{lambda}} as a symmetric function
    in {\tt R}.
  
  Example
    R = symmRing 10;
    jacobiTrudi({3,2,2,1},R,EorH => "E")
    jacobiTrudi(new Partition from {4,4,1},R)
    toS oo
///

doc///
   Key
     [jacobiTrudi,EorH]
   Headline
     e- or h- representation of Jacobi-Trudi determinant
   Usage
     EorH => s
   Inputs
     s:String
       either "E" or "H"
   Description
     Text
       This option allows one to choose between evaluating the
       Jacobi-Trudi determinant in the {\tt e}- or {\tt h}- basis.
       If the length of the conjugate partition {\tt lambda'} is
       smaller than the length of {\tt lambda}, then it is
       computationally less expensive to set the option {\tt EorH}
       to {\tt E}. Otherwise, the default value {\tt H} is more
       efficient.
///   

doc///
   Key
     [jacobiTrudi,Memoize]
   Headline
     Store values of the jacobiTrudi function.
   Usage
     Memoize => b
   Inputs
     b:Boolean
   Description
     Text
     
       If the option is set to {\tt true} then all the values of the jacobiTrudi 
       function that are computed are recorded into a special hash table attached
       to the symmetric ring inside which the computations are done.
///   

doc ///
Key
  plethysm
  (plethysm,RingElement,RingElement)
Headline
  Plethystic composition of symmetric functions/characters
Usage
  pl = plethysm(f,g)
Inputs
  f:RingElement
    element of Symmetric ring or Schur ring
  g:RingElement
    element of Symmetric ring or Schur ring
Outputs
  pl:RingElement
     element of Symmetric ring or Schur ring
Description
  Text
    Given two symmetric functions (or virtual characters of a general
    linear group) {\tt f} and {\tt g}, the method computes their
    plethystic composition. If {\tt g} is an element of a symmetric
    ring, then both {\tt f,g} are regarded as symmetric functions.
    Otherwise they are considered to be virtual characters. The result
    of the composition of {\tt f} and {\tt g} will be an element of
    the ring of {\tt g}.
    
  Example
    R = symmRing 10;
    p = plethysm(h_2,h_5)
    toS p
    S = schurRing(q,3);
    plethysm(h_2,q_{2,1})
    plethysm(q_{2,1},q_{2,1})
    
  Text
    
    An error is returned if {\tt g} is an element of a Symmetric ring {\tt R}
    and the product of the degrees of {\tt f} and {\tt g} is
    smaller than the dimension of {\tt R}.
///

doc ///
Key
  (plethysm,BasicList,RingElement)
Headline
  Plethystic composition of Schur function and symmetric function/character
Usage
  pl = plethysm(lambda,g)
Inputs
  lambda:BasicList
         nonincreasing sequence of positive integers, or partition
  g:RingElement
    element of Symmetric ring or Schur ring
Outputs
  pl:RingElement
     element of Symmetric ring or Schur ring
Description
  Text
    
    The method computes the plethystic composition of a Schur function
    corresponding to a partition {\tt lambda} and a symmetric function 
    or virtual character. This is the most commonly used form of plethysm.

  Example
    R = symmRing 6
    S = schurRing(q,3)
    plethysm({2,1},e_2)
    plethysm({2,1,1},q_{1,1}) 
///

doc ///
  Key
    Partitions
    (Partitions,Set,BasicList)
  Headline
    Partitions of a set
  Usage
    par = Partitions(S,L)
  Inputs
    S:Set
    L:BasicList
      a nonincreasing list of integers, or a partition
  Outputs
    par:List
  Description
    Text

      Given a set {\tt S} and a partition {\tt L=\{l_1\geq l_2\cdots\}}, the method
      returns the list of partitions of the set {\tt S} of type
      {\tt L}, i.e. representations of {\tt S} as {\tt S=S_1\cup S_2\cup\cdots}, where
      the {\tt S_i}'s are disjoint subsets of {\tt S} having {\tt t_i} elements.

    Example
      Partitions(set{1,2,3,4},{2,1,1})
      Partitions(set{a,b,c,d,e},new Partition from {3,2})
///  

doc ///
Key
  (chi,BasicList,BasicList)
Headline
  Irreducible character of symmetric group
Usage
  v = chi(lambda,rho)
Inputs
  lambda:BasicList
   	 a nondecreasing list of positive integers, or a partition
  rho:BasicList
      a nondecreasing list of positive integers, or a partition
Outputs
  v:QQ
Description
  Text

    Given two partitions {\tt lambda} and {\tt rho} of the integer {\tt N}, this method
    computes the value of the irreducible character of the symmetric group
    corresponding to the partition {\tt lambda} evaluated on
    any permutation of cycle-type {\tt rho}.

    The character of the trivial representation takes the value
    1 on any permutation:
  
  Example
    chi({4},{2,1,1})
    
  Text
  
    The character of the sign representation takes the value -1 on
    a cycle of length 4:
  
  Example
    chi({1,1,1,1},{4})
SeeAlso
   charToSym
   symToChar
///

doc ///
Key
  charToSym
  (charToSym,HashTable,Ring)
Headline
  Converts virtual character to symmetric function
Usage
  f = charToSym(ch,R)
Inputs
  ch:HashTable
  R:Ring
    a Symmetric ring
Outputs
  f:RingElement
    element of a Symmetric ring
Description
  Text
    Given a virtual character {\tt ch} of a
    symmetric group, and given a Symmetric ring {\tt R}, 
    the method computes the corresponding symmetric function
    as an element of {\tt R}.
    
  Example
    R = symmRing 4;
    charToSym(new HashTable from {{1,1,1,1}=>2},R)
SeeAlso
  symToChar
  chi
///

doc ///
Key
  symToChar
  (symToChar,RingElement)
Headline
  Converts symmetric function to virtual character
Usage
  ch = symToChar(f)
Inputs
  f:RingElement
    element of a Symmetric ring
Outputs
  ch:HashTable
Description
  Text
    Given a symmetric function {\tt f}, homogeneous of degree {\tt N}, 
    the method computes the corresponding virtual character
    of the symmetric group {\tt S_N}.
    
    The character of the standard representation of {\tt S_5} is
    
  Example
    R = symmRing 5;
    symToChar(jacobiTrudi({4,1},R))
  Text

    The character of the second exterior power of the standard representation of {\tt S_5} is
    
  Example
    R = symmRing 5;
    symToChar(jacobiTrudi({3,1,1},R))
SeeAlso
  charToSym
  chi
///

doc ///
Key
  scalarProd
Headline
  Standard pairing on symmetric functions/class functions
Description
  Text
  
    This method computes the standard scalar product on the ring {\tt \Lambda}
    of symmetric functions. One way to define this product is by imposing
    that the collection of Schur functions {\tt s_{\lambda}} form
    an orthonormal basis.
    
    Alternatively, by the correspondence between symmetric functions
    and virtual characters of symmetric groups, this scalar product
    coincides with the standard scalar product on class functions.

    The number of standard tableaux of shape {\tt \{4,3,2,1\}} is:
  
  Example
    R = symmRing 10;
    S = schurRing(s,4);
    scalarProd(h_1^10,s_{4,3,2,1})
SeeAlso
  intProd
///

doc ///
Key
  (scalarProd,RingElement,RingElement)
Headline
  Standard scalar product of symmetric functions
Usage
  sp = scalarProduct(f1,f2)
Inputs
  f1:RingElement
     element of a Symmetric Ring
  f2:RingElement
     element of a Symmetric Ring
Outputs
  sp:QQ
Description
  Text
    
    Given symmetric functions {\tt f1} and {\tt f2}, the method
    computes the standard pairing between {\tt f1} and {\tt f2}.
    
  Example
    R = symmRing 5;
    scalarProd(h_5,p_5)
    scalarProd(s_{4,1},p_5)
  
  Text
  
    Indeed, the coefficients of {\tt s_5} and {\tt s_{4,1}} in the
    s-basis expansion of {\tt h_5} are as computed above:
    
  Example
    R = symmRing 5;
    toS p_5
///

doc ///
Key
  (scalarProd,HashTable,HashTable)
Headline
  Standard scalar product of class functions
Usage
  sp = scalarProduct(ch1,ch2)
Inputs
  ch1:HashTable
  ch2:HashTable
Outputs
  sp:QQ
Description
  Text
    
    Given virtual characters {\tt ch1} and {\tt ch2}, the method
    computes the standard pairing between {\tt ch1} and {\tt ch2}.
    
  Example
    ch1 = new HashTable from {{3,2} => 2, {2,2,1} => -2, {3,1,1} => 2, {5} => 1};
    ch2 = new HashTable from {{2,2,1} => -2, {5} => 1, {1,1,1,1,1} => 5, {3,2} => 3, {4,1} => -2};
    scalarProd(ch1,ch2)
///

doc ///
Key
  intProd
Headline
  Internal product of symmetric functions/class functions
Description
  Text
  
    This method computes the internal (Kronecker) product of two homogeneous symmetric
    functions of the same degree. If we think of these functions as being
    virtual characters of some symmetric group, then their internal product
    is just the character of the tensor product of the corresponding virtual
    representations.

    The complete symmetric function of degree {\tt n} corresponds
    to the trivial {\tt S_n}-representation and is therefore
    the unit of the representation ring of {\tt S_n}:
  
  Example
    R = symmRing 5;
    S = schurRing(s,3);
    intProd(h_3,s_{2,1})
  Text
  
    The square of the sign representation is the trivial representation:
    
  Example
    R = symmRing 5;
    toH intProd(e_3,e_3)
SeeAlso
  scalarProd
///

doc ///
Key
  (intProd,RingElement,RingElement)
Headline
  Kronecker product of symmetric functions
Usage
  ip = intProd(f1,f2)
Inputs
  f1:RingElement
     element of a Symmetric ring or a Schur ring
  f2:RingElement
     element of a Symmetric ring or a Schur ring
Outputs
  ip:Ring
     a Symmetric ring or a Schur Ring
Description
  Text
    
    Given symmetric functions {\tt f1} and {\tt f2}, the method
    computes the Kronecker product {\tt ip} between {\tt f1} and {\tt f2}.
    The output {\tt ip} is an element in the ring of {\tt f2}.
    
  Example
     R = symmRing 6;
     S = schurRing(s,3);
     intProd(s_{3},e_3)
     Q = schurRing(q,4);
     intProd(s_{3,3},q_{4,2})   
  Text
   
    An error is returned if {\tt f1} and {\tt f2} don't have the
    same degree.
///

doc ///
Key
  (intProd,HashTable,HashTable)
Headline
  Tensor product of virtual representations
Usage
  ip = intProd(ch1,ch2)
Inputs
  ch1:HashTable
  ch2:HashTable
Outputs
  ip:HashTable
Description
  Text
    
    Given virtual characters {\tt ch1} and {\tt ch2}, the method
    computes the character of the tensor product of corresponding
    virtual representations of the symmetric group.
    
  Example
    ch1 = new HashTable from {{4,4} => 2, {8} => -1, {5,2,1} => 2, {3,2,2,1} => 1};
    ch2 = new HashTable from {{2,2,2,2} => -4, {5,2,1} => 1, {3,2,2,1} => 3};
    intProd(ch1,ch2)
///

doc ///
Key
  zee
  (zee,List)
Headline 
  Size of the centralizer of a permutation
Usage 
  n = zee(rho)
Inputs 
  rho:List
Outputs 
  n:ZZ
Description
  Text

    {\tt rho} is a list representing the cycle type of some permutation: the i-th entry in {\tt rho} is the number of cycles of length i of the permutation.
    The output of the function {\tt zee} is the size of the centralizer in the symmetric group of any permutation of cycle type {\tt rho}. The cycle type {\tt rho}
    corresponds to a partition {\tt lambda}, in which case {\tt zee(rho)} is also the value of the square norm of the symmetric function {\tt p_{lambda}}.

  Example
    zee{1,1,1}
    R = symmRing 6;
    u = p_1 * p_2 * p_3;
    scalarProd(u,u)
///

doc ///
  Key
    Memoize
  Headline
    Record values of the jacobiTrudi function
  Description
    Text
    
      This is an optional argument for the @TO jacobiTrudi@
      and @TO toS@ methods, allowing one to store the values
      of the @TO jacobiTrudi@ function in order to speed up
      computations.
///

doc ///
  Key
    Stillman
  Headline
    Strategy for finding the s-basis representation of a symmetric function
  Description
    Text
      This is one of the two strategies implemented for the 
      @TO toS@ method that computes a representation of
      a symmetric function in the basis of Schur functions. 
  SeeAlso
    Stembridge
///

doc ///
  Key
    Stembridge
  Headline
    Strategy for finding the s-basis representation of a symmetric function
  Description
    Text
      This is one of the two strategies implemented for the 
      @TO toS@ method that computes a representation of
      a symmetric function in the basis of Schur functions. 
  SeeAlso
    Stillman
///

doc ///
Key
  cauchy
Headline
  Exterior power of a tensor product of representations
Usage
  l = cauchy(r,f,g)
Inputs
  r:ZZ
  f:RingElement
    element of a Schur ring
  g:RingElement
    element of a Schur ring
Outputs
  l:List
Description
  Text
    
    Given representations {\tt V,W} over (possibly different)
    general linear groups {\tt G,H}, with corresponding characters {\tt f,g},
    and given any positive integer {\tt r}, the method computes
    the character (over {\tt G\times H}) of the {\tt r}-th exterior 
    power of {\tt V\otimes W}.

  Example
    A = schurRing(a,4);
    B = schurRing(b,5);
    cauchy(3,a_{1},b_{1})
  Text    
    
    The output {\tt l} is a list of pairs of characters {\tt f_j,g_j}
    corresponding to representations {\tt V_j,W_j} such that the direct
    sum of {\tt V_j\otimes W_j} is the {\tt r}-th exterior power of
    {\tt V\otimes W}.
    
Caveat
  This is terrible. Once we make Schur rings work over arbitrary
  base rings we'll be able to work with tensor products of
  representation rings, and the elements of these rings will replace
  the lists of pairs as outputs of this function.
SeeAlso
  wedge
///

doc ///
Key
  wedge
  (wedge,ZZ,List)
Headline
  Exterior power of a representation over a product of general linear groups
Usage
  l = wedge(r,L)
Inputs
  r:ZZ
  L:List
    list of pairs of elements of (possibly different) Schur ring
Outputs
  l:List
Description
  Text
    
    This is the general case of @TO cauchy@. Given a representation {\tt U}
    over a product of general linear groups {\tt G,H},
    and given any positive integer {\tt r}, the method computes
    the character (over {\tt G\times H}) of the {\tt r}-th exterior
    power of {\tt U}. {\tt U} is given as a list of pairs of characters {\tt f_j,g_j}
    corresponding to representations {\tt V_j,W_j} such that {\tt U} is the direct
    sum of {\tt V_j\otimes W_j}.

  Example
    A = schurRing(a,4);
    B = schurRing(b,5);
    wedge(2,{(a_{1},b_{1}),(a_{2}+a_{1,1},b_{3})})
  Text    
    
    The output {\tt l} is a list of pairs of characters {\tt f_j,g_j}
    corresponding to representations {\tt V_j,W_j} such that the direct
    sum of {\tt V_j\otimes W_j} is the {\tt r}-th exterior power of
    {\tt U}.
    
Caveat
  This is terrible. See @TO cauchy@ for details.
SeeAlso
  cauchy
///

--------------------
-- test Jacobi-Trudi
--------------------
TEST ///
E = symmRing 5
f = jacobiTrudi({4,1},E)
assert (toS f == s_{4,1})
///

TEST ///
E = symmRing 5
f = jacobiTrudi({2,1},E)
assert (toS f == s_{2,1})
///

TEST ///
E = symmRing 13
f = jacobiTrudi({7,4,2},E)
assert (toS f == s_{7,4,2})
///

TEST ///
P = symmRing 6
f = toS plethysm(jacobiTrudi({2},P), jacobiTrudi({2},P))
assert(f == s_{4}+s_{2,2})
///

TEST ///
Q = symmRing 6
S = schurRing(q,4)
f = toS(plethysm(jacobiTrudi({3},Q), jacobiTrudi({2},Q)),S)
assert(dim f == 220)
///
------------------------
-- end test Jacobi-Trudi
------------------------

------------------------
-- test of plethysm, toS
------------------------
TEST ///
R = symmRing 5
pl = plethysm({1,1},jacobiTrudi({2},R))
assert(toS pl == s_{3,1})
///

TEST ///
R = symmRing 12
pl = plethysm({1,1,1},jacobiTrudi({4},R))
assert(#terms(toS pl) == 7)
///

TEST ///
R = symmRing 9
S = schurRing(q,3)
pl = plethysm(h_3,q_{2,1})
assert (dim pl == 120)
///

TEST ///
R = symmRing 10
S = schurRing(s,3)
assert(toS(plethysm(h_3,e_3),S) == s_{3,3,3})
///

TEST ///
S = schurRing(q,4)
assert(plethysm(q_{2,1},q_{1,1,1}) == q_{3,3,2,1})
///

TEST ///
R = symmRing 12
f = e_4
lambda = new Partition from {3}
plethysm(lambda,f) == plethysm(h_3,e_4)
///

TEST ///
schurRing(s,2,CoefficientRing => QQ)
assert(dim(plethysm(s_{2,1}+s_{3},s_{3})) == 40)
///

TEST ///
R = symmRing 20
assert(#terms(toS(plethysm(h_5,h_4),Memoize=>true)) == 95)
///

TEST ///
R = symmRing 10
S = schurRing(o,5,CoefficientRing => QQ)
sch = toS(plethysm({2,1},h_3),S,Strategy => Stillman,Memoize => false)
assert(dim sch == 14770)
///
----------------------------
-- end test of plethysm, toS
----------------------------

--------------------------------------------------------------
----- test characters of symmetric groups, scalarProd, intProd
--------------------------------------------------------------
TEST ///
assert(chi({2,1,1,1},{2,1,1,1}) == -2)
assert(chi({3,1,1},{1,1,1,1,1}) == 6)
assert(chi({3,2},{3,1,1}) == -1)
assert(chi({2,2,1},{3,1,1}) == -1)
assert(chi({3,1,1},{2,2,1}) == -2)
///

TEST ///
R = symmRing 20
S = schurRing(o,6)
assert(scalarProd(o_{6,4,3,2,1},jacobiTrudi({3,3,3},R)*toP(o_{4,2,1},R)) == 2)
assert(scalarProd(jacobiTrudi({6,4,3,2,1},R),jacobiTrudi({4,3,3,3,2,1},R)) == 0)
assert(scalarProd(jacobiTrudi({6,4,3,2,1},R),o_{4,3,3,3,2,1}) == 0)
///

TEST ///
R = symmRing 5
A = schurRing(a,2)
assert(intProd(e_2+h_2,a_{2}) == a_{2}+a_{1,1})
assert(toE intProd(a_{2},e_2+h_2) == toE p_1^2)
assert(dim intProd(a_{2,1}*a_{1},a_{2,2}) == 9)
///


TEST ///
R = symmRing 10
ch1 = new HashTable from {{4,4} => 2, {8} => -1, {5,2,1} => 2, {3,2,2,1} => 1};
ch2 = new HashTable from {{2,2,2,2} => -4, {5,2,1} => 1, {3,2,2,1} => 3};
assert(toP charToSym(intProd(ch1,ch2),R) == 1/8*p_1*p_2^2*p_3+1/5*p_1*p_2*p_5)
///

TEST ///
R = symmRing 4
f = p_2^2
g = (e_2+h_2)^2
ch1 = symToChar(f)
ch2 = symToChar(g)
charToSym(intProd(ch1,ch2),R) == 0
intProd(f,g) == 0
///
----------------------------------------------------------------
--- end test characters of symmetric groups, scalarProd, intProd
----------------------------------------------------------------

---------------------------
--- test toS, toP, toE, toH
---------------------------
TEST ///
R = symmRing 6
assert(toE(toS(e_1*e_2*e_3),R) == e_1*e_2*e_3)
///

TEST ///
R = symmRing 5
S = schurRing(q,3)
assert(toE(q_{2},R) + e_2 == e_1^2)
///

TEST///
R = symmRing 4
assert(toP toE toH toE toH toP toE toE toP toH (p_1+p_2+p_3) == p_1+p_2+p_3)
///

TEST ///
R = symmRing 6
S = schurRing(o,2)
toSf = map(S, R, apply(gens R, x -> toS(x,S)))
assert(toSf(e_1*e_2*e_3) == 0)
assert(toSf(h_1*h_2*h_3) == o_{1}*o_{2}*o_{3})
///

TEST ///
R = symmRing 7
toH toP toE (toS (jacobiTrudi({2,1},R))^2,R) == (h_1*h_2-h_3)^2
///
-------------------------------
--- end test toS, toP, toE, toH
-------------------------------

---------------------------
-- test of cauchy, wedge --
---------------------------

TEST ///
R = symmRing 6
A = schurRing(a,1)
B = schurRing(b,2)

x = cauchy(2,a_{1},b_{2})
y = toS(plethysm(R_1,R_13),B)
assert(x#0#1 == y)

x = cauchy(3,a_{1},b_{2})
y = toS(plethysm(R_2,R_13),B)
assert(x#0#1 == y)
///

TEST ///
A = schurRing(a,2)
B = schurRing(b,2)
L = {(a_{2},b_{1}),(a_{1,1},b_{1,1})}
assert(#(set(wedge(2,L)) - (set cauchy(2,a_{2},b_{1}) + set{(a_{2}*a_{1,1},b_{1}*b_{1,1})})) == 0)
///
-------------------------------
-- end test of cauchy, wedge --
-------------------------------
end

-----------------------------------------------------------------------------
-- the rest of this file used to be schur.m2
-----------------------------------------------------------------------------

-- cauchy = (i,f,g) -> (
--      -- f and g are elements of symmRing's (possibly different)
--      -- compute the i th exterior power of the representation f ** g
--      P := partitions i;
--      n := (ring f).dim;
--      n' := (ring g).dim;
--      result := apply(P, lambda -> (
-- 	       if #lambda > n or lambda#0 > n' then null
-- 	       else (
-- 		    plethysm(lambda, f),
-- 	     	    plethysm(conjugate lambda, g))
-- 		    ));
--      select(result, x -> x =!= null)
--      )

cauchy = (i,f,g) -> (
     -- f and g are elements of symmRing's (possibly different)
     -- compute the i th exterior power of the representation f ** g
     P := partitions i;
     n := (ring f).dim;
     n' := (ring g).dim;
     result := apply(P, lambda -> (
	       --if #lambda > n or lambda#0 > n' then null
	       --else 
	       (
		   a := plethysm(lambda,f);
		   if a == 0 then null
		   else (
			b := plethysm(conjugate lambda, g);
			if b == 0 then null else (a,b)
		    ))));
     select(result, x -> x =!= null)
     )

compositions1 = (r,n) -> (
     -- return a list of all of the n-compositions of r.
     -- i.e. ways to write r as a sum of n nonnegative integers
     if n === 1 then {{r}}
     else if r === 0 then {toList(n:0)}
     else (
	  flatten for i from 0 to r list (
	       w := compositions1(r-i,n-1);
	       apply(w, w1 -> prepend(i,w1)))))


pairProduct = L -> (
     -- L is a list of lists of (f,g), f,g both in symm rings.
     -- result: a list of pairs (f,g).
     if #L === 1 then first L
     else (
	  L' := drop(L,1);
	  P' := pairProduct L';
	  flatten apply(L#0, fg -> (
	       (f,g) := fg;
	       apply(P', pq -> (
		    (p,q) := pq;
		    (f*p, g*q)))))
     ))
----e.g. L = {{(h_2,e_3)},{(h_1,e_3),(p_2,e_2)}}
----pairProduct L = {(h_1*h_2,e_3^2), (p_2*h_2,e_2*e_3)}

wedge = method()		    
wedge(List,List) := (C,L) -> (
     -- MES MES: we are working on this function now.  It is fucked up.
     -- the plethysmMap seems messed up.  We really need to make a routine
     -- plethysm(partition,representation)
     -- MES MES
     -- C is a composition of 0..n-1, n == #L
     -- form the product of the exterior powers of the corresponding representations.
     result := {}; -- each entry will be of the form (f,g)
     C0 := positions(C, x -> x =!= 0);
     wedgeL := flatten apply(C0, i -> (
	       apply(L#i, fg -> cauchy(C#i,fg#0,fg#1))));
     pairProduct wedgeL     
     )

wedge(ZZ,List,List) := (r,L,ranks) -> (
     -- r is an integer >= 1
     -- L is a list of pairs (f,g), f,g are in (possibly different) symm rings.
     -- returns wedge(r)(L), as a sum of irreducible representations of GL(m) x GL(n)
     n := #L;
     p := compositions1(r,n);
     p = select(p, x -> all(ranks-x, i -> i>=0));
     join apply(p, x -> wedge(x,L))
     )

bott = (QRreps) -> (
     -- returns a list of either: null, or (l(w), w.((Qrep,Rrep)+rho) - rho)
     s := QRreps; -- join(Qrep,Rrep);
     rho := reverse toList(0..#s-1);
     s = s + rho;
     len := 0;
     s = new MutableList from s;
     n := #s;
     for i from 0 to n-2 do
     	  for j from 0 to n-i-2 do (
	       if s#j === s#(j+1) then return null;
	       if s#j < s#(j+1) then (
		    tmp := s#(j+1);
		    s#(j+1) = s#j;
		    s#j = tmp;
		    len = len+1;
		    )
	       );
     (len, toList s - rho)
     )

preBott = (i,L,ranks) -> (
     R1 := ring L#0#0#0;
     R2 := ring L#0#0#1;
     dimQ := R1.dim; -- for general bundles we will need to know the ranks concerned
     dimR := R2.dim;
     x := flatten wedge(i,L,ranks);
     x = apply(x, x0 -> (toS x0#0, toS x0#1));
     B := new MutableHashTable;
     scan(x, uv -> (
	       (u,v) := uv;
	       scan(u, u0 -> (
			 scan(v, v0 -> (
			       pQ := u0#0;
			       pR := v0#0;
			       if #pQ < dimQ then
			         pQ = join(pQ,toList((dimQ-#pQ):0));
			       if #pR < dimR then
			         pR = join(pR,toList((dimR-#pR):0));
			       b := join(pQ,pR);
			       c := u0#1 * v0#1;
			       if B#?b then B#b = B#b + c
			       else B#b = c))))));
     B)

doBott = (nwedges,B) -> (
     -- B is the output of preBott
     kB := keys B;
     S := Schur (#(first kB));
     apply(keys B, x -> (
	       b := bott x;
	       if b === null then null
	       else (
		    glb = b#1;
		    d := B#x * dim S_(b#1);
		    (b#0, nwedges - b#0, b#1, B#x, d)))))


weyman = (i,L,ranks) -> (
     B := preBott(i,L,ranks);
     doBott(i,B))

end

-----------------------------------------------------------------------------
-- some tests that can be incorporated into the documentation later
-----------------------------------------------------------------------------

--------------------
-- test of cauchy --
--------------------
restart
load "schur.m2"
R1 = symmRing 5
f = jacobiTrudi({2},R1)
g = jacobiTrudi({1},R1)
cauchy(2,f,g)
cauchy(3,f,g)
R1 = symmRing 1
R2 = symmRing 2
cauchy(2,jacobiTrudi({1},R1),jacobiTrudi({2},R2))
toS oo_0_1
cauchy(3,1_R1,jacobiTrudi({3},R2))
toS oo_0_1


--------------------
-- test of bott ----
--------------------
restart
load "schur.m2"
R1 = symmRing 5
bott({0,3,0}) == (1, {2, 1, 0})
bott({1,2,0}) === null
bott({2,1,0}) == (0, {2, 1, 0})
bott({2,4,0}) == (1, {3, 3, 0})
bott({0,3,3}) == (2, {2, 2, 2})
bott({3,2,1,20,10,1})
--------------------
-- test of pairProduct
--------------------
restart
load "schur.m2"
R1 = symmRing 1
R2 = symmRing 2
L = {{(1_R1, jacobiTrudi({3},R2))}, 
     {(jacobiTrudi({1},R1), jacobiTrudi({2},R2))}, 
     {(jacobiTrudi({2},R1), jacobiTrudi({1},R2))}}
pairProduct L
L1 = drop(L,1)
pairProduct L1
wedge({2,1,0},L)
c1 = cauchy(3,L#0#0#0, L#0#0#1)
c2 = cauchy(2,L#1#0#0, L#1#0#1)
(c1_0)/toS
(c2_0)/toS
(c2_1)/toS
wedge({3,2,0},L)
L#0#0#0
L#0#0#1

L/(v -> (toS v#0#1))


--------------------
-- test of wedge
--------------------
restart
loadPackage "SchurRings"
R1 = symmRing 3
R2 = symmRing 3
L = {{(1_R1, jacobiTrudi({3},R2))}, 
     {(jacobiTrudi({1},R1), jacobiTrudi({2},R2))}, 
     {(jacobiTrudi({2},R1), jacobiTrudi({1},R2))}}
wedge({3,2,0},L)
wedge({3,0,0},L)
z = preBott(1,L,{4,3,2})
doBott(5,z)

weyman(1,L,{4,3,2})
weyman(2,L,{4,3,2})
weyman(3,L,{4,3,2})
weyman(4,L,{4,3,2})
weyman(5,L,{4,3,2})
weyman(6,L,{4,3,2})
weyman(7,L,{4,3,2})
weyman(8,L,{4,3,2})
weyman(9,L,{4,3,2})
--------------------
restart
load "schur.m2"
R1 = symmRing 1
R2 = symmRing 3
L = {{(1_R1, jacobiTrudi({4},R2))}, 
     {(jacobiTrudi({1},R1), jacobiTrudi({3},R2))}, 
     {(jacobiTrudi({2},R1), jacobiTrudi({2},R2))},
     {(jacobiTrudi({3},R1), jacobiTrudi({1},R2))}}
wedge({3,2,0},L)
wedge({3,0,0},L)
z = preBott(1,L,{4,3,2})
doBott(5,z)

weyman(1,L,{15,10,6,3})
weyman(2,L,{15,10,6,3})
weyman(3,L,{15,10,6,3})
weyman(4,L,{15,10,6,3})
weyman(7,L,{15,10,6,3})
weyman(15,L,{15,10,6,3})

y

weyman(2,L,{4,3,2})
weyman(3,L,{4,3,2})
weyman(4,L,{4,3,2})
weyman(5,L,{4,3,2})
weyman(6,L,{4,3,2})
weyman(7,L,{4,3,2})
weyman(8,L,{4,3,2})
weyman(9,L,{4,3,2})
	       
--------------------
#(compositions1(9,3))

n = 3
--R = QQ[x_1 .. x_n]
p = symbol p
R = QQ[x_1 .. x_n, p_1 .. p_n, MonomialOrder => ProductOrder {n,n}]
slambda = (lambda) -> (
     map(R^n, n, (i,j) -> x_(i+1)^(lambda#j+n-j-1))
     )

schur = (lambda) -> (
     p := toList(#lambda:0);
     det slambda(lambda) // det slambda p)

end

-- example to debug
restart
A = frac(QQ[a,b])
T = schurRing(p,3,CoefficientRing=>A)
a*p_{2,1}

S1 = schurRing(a,3)
S1a = toField S1
S2 = schurRing(b,4,CoefficientRing=>S1a)
-------------------
restart
uninstallPackage "SchurRings"
installPackage "SchurRings"
check SchurRings
viewHelp SchurRings
--print docTemplate
end


-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages PACKAGES=SchurRings pre-install"
-- End: