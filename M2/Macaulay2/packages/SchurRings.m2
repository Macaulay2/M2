---*- coding: utf-8 -*-
--------------------------------------------------------------------------------
-- Copyright 2007, 2011 Michael Stillman
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU General Public License as published by the Free Software
-- Foundation, either version 3 of the License, or (at your option) any later
-- version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
-- details.
--
-- You should have received a copy of the GNU General Public License along with
-- this program.  If not, see <http://www.gnu.org/licenses/>.
--------------------------------------------------------------------------------

newPackage(
	"SchurRings",
    	Version => "1.1", 
    	Date => "August 24, 2011",
    	Authors => {
	     {Name => "Michael Stillman", Email => "mike@math.cornell.edu", HomePage => "http://www.math.cornell.edu/~mike/"},
	     {Name => "Hal Schenck"},
	     {Name => "Claudiu Raicu", Email => "claudiu@math.berkeley.edu", HomePage => "http://math.berkeley.edu/~claudiu/"}
	     },
	Keywords => {"Representation Theory"},
    	Headline => "representation rings of general linear groups and of symmetric groups"
--	AuxiliaryFiles => true
    	)

export {"schurRing", "SchurRing", "symmetricRing",
     "toS", "toE", "toP", "toH",
     "jacobiTrudi", "plethysm",
     "centralizerSize", "classFunction", "symmetricFunction", 
     "scalarProduct", "internalProduct",
     "SchurRingIndexedVariableTable", "EHPVariables", "SVariable",
     "ClassFunction", "schurLevel",
     "schurResolution",
     "SchurRingElement",     
     "Memoize", "Schur", "EorH", "GroupActing",
     "eVariable", "pVariable", "hVariable"
     }

debug Core

protect symbol symRingForE;
protect symbol mapToE;
protect symbol symRingForP;
protect symbol mapToP;
protect symbol mapFromP;
protect symbol grbE
protect symbol PtoETable
protect symbol HtoETable
protect symbol grbH
protect symbol PtoHTable
protect symbol EtoHTable
protect symbol grbP
protect symbol EtoPTable
protect symbol HtoPTable
--protect symbol plethysmMaps
protect symbol mapFromE
protect symbol sFunction


SchurRing = new Type of EngineRing
SchurRing.synonym = "Schur ring"
ClassFunction = new Type of HashTable
ClassFunction.synonym = "Class function"

expression SchurRing := S -> new FunctionApplication from { schurRing, (expression last S.baseRings, S.Symbol, S.numgens ) }
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

rawmonom2partition = (m) -> (
     reverse splice apply(rawSparseListFormMonomial m, (x,e) -> e:x)
     )

--various ways of addressing elements of a Schur ring
SchurRing _ List := (SR, L) -> new SR from rawSchurFromPartition(raw SR, L)
SchurRing _ Sequence := (SR, L) -> new SR from rawSchurFromPartition(raw SR, L)
SchurRing _ ZZ := (SR, L) -> new SR from rawSchurFromPartition(raw SR, 1:L)
--
coefficientRing SchurRing := Ring => R -> last R.baseRings
numgens SchurRing := Ring => R -> R.numgens

--n = schurLevel R is the number of iterations of the schurRing/symmetricRing function
--used in the construction of R
schurLevel = method()
schurLevel (Ring) := R -> if R.?schurLevel then R.schurLevel else 0

--Construction of Schur rings
newSchur2 = method()
newSchur2(Ring,Symbol) := (A,p) -> newSchur2(A,p,-1)

SchurRingElement = new Type of RingElement
newSchurEngineRing = R -> (
     S := new SchurRing of SchurRingElement;
     S.RawRing = R;
     S#1 = 1_S;
     S#0 = 0_S;
     S)
       
newSchur2(Ring,Symbol,ZZ) := (A,p,n) -> (
     if not (A.?Engine and A.Engine) 
     then error "expected coefficient ring handled by the engine";
     SR := newSchurEngineRing rawSchurRing1(raw A,n);
     SR.Symbol = p;
     SR.baseRings = append(A.baseRings,A);
     SR.generators = {};
     SR.numgens = if n < 0 then infinity else n;
     SR.degreeLength = 0;
--the basic features of SR are coded at the engine level
     commonEngineRingInitializations SR;
     ONE := SR#1;
     if A.?char then SR.char = A.char;
     toExternalString SR := r -> toString expression r;
     expression SR := f -> (
	  (coeffs,monoms) -> sum(
	       coeffs,monoms,
	       (a,m) -> expression (if a == 1 then 1 else new A from a) *
	          new Subscript from {p, (
		    t1 := toSequence rawmonom2partition m;
		    if #t1 === 1 then t1#0 else t1
		    )})
	  ) rawPairs(raw A, raw f);
     listForm SR := (f) -> (
     	  n := numgens SR;
     	  (cc,mm) := rawPairs(raw A, raw f);
     	  toList apply(cc, mm, (c,m) -> (rawmonom2partition m, new A from c)));
     if (A.?schurLevel) then SR.schurLevel = A.schurLevel + 1
     else SR.schurLevel = 1;
     SR
     )

schurRing = method(Options => {EHPVariables => (getSymbol"e",getSymbol"h",getSymbol"p"), SVariable => getSymbol"s", GroupActing => "GL"})
schurRing(Ring,Thing,ZZ) := SchurRing => opts -> (A,p,n) -> (
     try p = baseName p else error "schurRing: can't use provided thing as variable";
     if class p === Symbol then schurRing(A,p,n,opts)
     else error "schurRing: can't use provided thing as variable"
     );
schurRing(Ring,Thing) := SchurRing => opts -> (A,p) -> (
     try p = baseName p else error "schurRing: can't use provided thing as variable";
     if class p === Symbol then schurRing(A,p,opts)
     else error "schurRing: can't use provided thing as variable"
     );

dim SchurRingElement := s -> dimSchur s;
dim(List,SchurRingElement) := (lis,s) -> dimSchur(lis, s);
dim(Thing,SchurRingElement) := (n,s) -> dimSchur(n, s);


schurRing(Ring,Symbol) := opts -> (R,p) -> schurRing(R,p,infinity,opts)
schurRing(Ring,Symbol,InfiniteNumber) := 
schurRing(Ring,Symbol,ZZ) := SchurRing => opts -> (R,p,n) -> (
     S := local S;
     if n == infinity then S = newSchur2(R,p,-1) else S = newSchur2(R,p,n);
     S.EHPVariables = opts.EHPVariables;
     --S.SVariable = opts.SVariable;
     S.GroupActing = opts.GroupActing;
     S @ RingElement := RingElement @ S := (f1,f2) -> plethysm(f1,f2);
     S^ZZ := (f,n) -> product apply(n,i->f);
     symmetricPower(ZZ,S) := (n,s) -> plethysm({n},s);
     exteriorPower(ZZ,S) := opts -> (n,s) -> plethysm(splice{n:1},s);
     
     --define the multiplication on S
     --in the case when the group acting is a general linear group     
     if opts.GroupActing == "GL" then
     (
	  oldmult := method();
	  oldmult(S,S) := (f1,f2) -> new S from raw f1 * raw f2;
	  oldmult(RingElement, S) := (f1,f2) -> if member(ring f1,S.baseRings | {S}) then oldmult(promote(f1,S),f2);
	  oldmult(S, RingElement) := (f1,f2) -> if member(ring f2,S.baseRings | {S}) then oldmult(f1,promote(f2,S));
	  oldmult(Number, S) := (f1,f2) -> if member(ring f1,S.baseRings | {S}) then oldmult(promote(f1,S),f2);
	  oldmult(S, Number) := (f1,f2) -> if member(ring f2,S.baseRings | {S}) then oldmult(f1,promote(f2,S));

       	  S * S := (f1,f2) ->
	        if schurLevel S == 1 then oldmult(f1,f2)
	  	else
		  (
		       lF1 := listForm f1;
		       lF2 := listForm f2;
		       sum flatten for p1 in lF1 list
		       	    for p2 in lF2 list
			    	 (
				      oldmult((last p1) * (last p2), oldmult(S_(first p1),S_(first p2)))
				      )
		       );
	  )
     --define the multiplication on S
     --in the case when the group acting is a symmetric group
     else if opts.GroupActing == "Sn" then
     (
     	  S ** S := (f1,f2) -> new S from raw f1 * raw f2;
--	  RingElement ** S := (f,g) -> if member(ring f,S.baseRings | {S}) then promote(f,S) ** g;
--	  S ** RingElement := (f,g) -> if member(ring g,S.baseRings | {S}) then f ** promote(g,S);
	  RingElement ** S := (f,g) -> if member(ring f,S.baseRings | {S}) then promote(f,S) ** g
	       	    	      	   	else if member(S,(ring f).baseRings | {ring f}) then f ** promote(g,S);
	  Number ** S := (f,g) -> if member(ring f,S.baseRings | {S}) then promote(f,S) ** g;
	  S ** Number := (f,g) -> if member(ring g,S.baseRings | {S}) then f ** promote(g,S);
	       	  
       	  S * S := (f1,f2) ->
	        if schurLevel S == 1 then
	  	  (
	       	       cS := coefficientRing S;
	       	       if liftable(f1,cS) or liftable(f2,cS) then f1 ** f2 else
	       	       if f1 == 0 or f2 == 0 then 0_S else
	       	       internalProduct(f1,f2)
	       	       )
	  	else
		  (
		       lF1 := listForm f1;
		       lF2 := listForm f2;
		       sum flatten for p1 in lF1 list
		       	    for p2 in lF2 list
			    	 (
				      ((last p1) * (last p2)) ** internalProduct(S_(first p1),S_(first p2))
				      )
		       );
	  );

     t := new SchurRingIndexedVariableTable from p;
     t.SchurRing = S;
     t#symbol _ = a -> ( S _ a);
     S.use = S -> (globalAssign(p,t); S);
     S.use S;
     S)

--constructs the Schur ring of a symmetric ring R
--this is a ring with basis consisting of s-polynomials (Schur functions) that
--is abstractly isomorphic to R
--schurRingOf = method()
--schurRingOf (Ring) := R -> (
schurRing (Ring) := opts -> R -> (
     	  if R.?Schur then R.Schur else
	  if schurLevel R > 0 then
	  (
	       if instance(R, SchurRing) then R else
	       (
		    s := R.SVariable;
     	       	    if schurLevel R == 1 then R.Schur = schurRing(coefficientRing R,s,R.dim,EHPVariables => R.EHPVariables, GroupActing => R.GroupActing)
		       else R.Schur = schurRing(schurRing coefficientRing R,s,R.dim,EHPVariables => R.EHPVariables, GroupActing => R.GroupActing); --symmetricRing is wrong, right?
     	       	    R.Schur.symmetricRing = R;
	       	    R.Schur
		    )
	       )
	  else error"Expected ring to have a Schur Ring"
     )

schurRing(Thing,ZZ) := opts -> (s,n) -> schurRing(QQ,s,n,opts)
schurRing(Thing,InfiniteNumber) := opts -> (s,n) -> schurRing(QQ,s,n,opts)
schurRing(Thing) := opts -> (s) -> schurRing(QQ,s,-1,opts)

undocumented (schurRing,Ring,Symbol,InfiniteNumber)
undocumented (schurRing,Thing,InfiniteNumber)

--a new type that indexes the elements in the s-basis of a Schur ring
SchurRingIndexedVariableTable = new Type of IndexedVariableTable
SchurRingIndexedVariableTable _ Thing := (x,i) -> x#symbol _ i

--construction of symmetric rings
symmetricRing = method(Options => options schurRing)
symmetricRing (Ring,ZZ) := opts -> (A,n) -> (
     	  (e,h,p) := opts.EHPVariables;
     	  R := A[e_1..e_n,p_1..p_n,h_1..h_n,
	    Degrees => toList(1..n,1..n,1..n), MonomialSize => 8];
     	  R.EHPVariables = opts.EHPVariables;
	  R.SVariable = opts.SVariable;
       	  R.eVariable = (i) -> if 1 <= i and i <= n then R_(i-1) else error"Invalid index";
       	  R.pVariable = (i) -> if 1 <= i and i <= n then R_(n+i-1) else error"Invalid index";
       	  R.hVariable = (i) -> if 1 <= i and i <= n then R_(2*n+i-1) else error"Invalid index";
     	  R.GroupActing = opts.GroupActing;
     	  R.dim = n;
	  R ** R := (f1,f2) -> internalProduct(f1,f2); --internal product of symmetric functions
     	  R @ RingElement := RingElement @ R := (f1,f2) -> plethysm(f1,f2);
     	  symmetricPower(ZZ,R) := (n,r) -> plethysm({n},r);
     	  exteriorPower(ZZ,R) := opts -> (n,r) -> plethysm(splice{n:1},r);
--the degrees of e_i,p_i,h_i are equal to i 
	  degsEHP := toList(1..n);
--blocks#0 are indices for e-variables
--blocks#1 are indices for p-variables
--blocks#2 are indices for h-variables
     	  blocks := {toList(0..(n-1)),toList(n..(2*n-1)),toList(2*n..(3*n-1))};
--new variables for the E,H,P polynomials
     	  vrs := symbol vrs;
     	  locVarsE := apply(blocks#0,i->vrs_i);
     	  locVarsP := apply(blocks#1,i->vrs_i);
     	  locVarsH := apply(blocks#2,i->vrs_i);
--new rings used for conversion to E- and P- polynomials
--they differ from R in the order of the variables
--R is used by default for conversion to H-polynomials
          R.symRingForE = A[locVarsH | locVarsP | locVarsE ,Degrees=>flatten toList(3:degsEHP),MonomialOrder=>GRevLex, MonomialSize => 8];
     	  R.mapToE = map(R.symRingForE,R,apply(blocks#2|blocks#1|blocks#0,i->(R.symRingForE)_i));
     	  R.mapFromE = map(R,R.symRingForE,apply(blocks#2|blocks#1|blocks#0,i->R_i));
     	  R.symRingForP = A[locVarsH | locVarsE | locVarsP,Degrees=>flatten toList(3:degsEHP),MonomialOrder=>GRevLex, MonomialSize => 8];
     	  R.mapToP = map(R.symRingForP,R,apply(blocks#1|blocks#2|blocks#0,i->(R.symRingForP)_i));
     	  R.mapFromP = map(R,R.symRingForP,apply(blocks#2|blocks#0|blocks#1,i->R_i));
--compute conversion tables
--between E-,H- and P- polynomials
     	  EtoP(n,R);
     	  PtoE(n,R);
     	  HtoE(n,R);
     	  EtoH(n,R);
     	  PtoH(n,R);
     	  HtoP(n,R);
--define Groebner bases used for conversion between E-,H- and P- polynomials
     	  R.grbE = forceGB matrix(R.symRingForE, {flatten apply(splice{1..n},i->{R.mapToE(R_(n-1+i))-R.PtoETable#i,R.mapToE(R_(2*n-1+i))-R.HtoETable#i})});
     	  R.grbH = forceGB matrix(R, {flatten apply(splice{1..n},i->{R_(n-1+i)-R.PtoHTable#i,R_(-1+i)-R.EtoHTable#i})});
     	  R.grbP = forceGB matrix(R.symRingForP, {flatten apply(splice{1..n},i->{R.mapToP(R_(-1+i))-R.EtoPTable#i,R.mapToP(R_(2*n-1+i))-R.HtoPTable#i})});
     	  collectGarbage();
--construct maps that convert a polynomial in the E-,H-,P- variables
--into one involving only one of the three variables
     	  R.mapSymToE = (f) -> R.mapFromE(R.mapToE(f)%R.grbE);
     	  R.mapSymToP = (f) -> R.mapFromP(R.mapToP(f)%R.grbP);
     	  R.mapSymToH = (f) -> f%R.grbH;
--the Schur level of R is one more than that of its base ring
	  if (A.?schurLevel) then R.schurLevel = A.schurLevel + 1
     	  else R.schurLevel = 1;
     	  R)

--constructs the symmetric ring of a Schur ring 
--if the Schur ring has dimension n, its symmetric ring is the polynomial ring
--in the variables e_1,...,e_n,p_1,...,p_n,h_1,...,h_n, i.e. the other types of
--symmetric functions (besides the Schur functions) that the package implements
symmetricRing (Ring) := opts -> R -> (
     	  if R.?symmetricRing then R.symmetricRing else
	  if class R === SchurRing then
	  (
	       if numgens R === infinity then 
	          error"symmetric ring expects finite schurRings";
     	       if coefficientRing R === ZZ then
	       	  error"base ring has to be QQ";
     	       R.symmetricRing = symmetricRing(symmetricRing coefficientRing R,numgens R,EHPVariables => R.EHPVariables, SVariable => R.Symbol, GroupActing => R.GroupActing);
     	       R.symmetricRing.Schur = R;
	       R.symmetricRing
	       )
	  else R
     )

symmetricRing(ZZ) := opts -> n -> symmetricRing(QQ,n,opts)

---------------------------------------------------------------
--------------Jacobi-Trudi-------------------------------------
---------------------------------------------------------------

----local variables for jacobiTrudi
----they are used in the recursive function jT
auxR = local auxR;
auxn = local auxn;
auxEH = local auxEH;
----

jacobiTrudi = method(Options => {Memoize => true, EorH => "E"})
jacobiTrudi(BasicList,Ring) := opts -> (lambda,R) ->
(
     lam := new Partition from lambda;
     rez := local rez;
     local u;
     if opts.EorH == "H" then u = R.hVariable else (u = R.eVariable;lam = conjugate lam;);
     if opts.Memoize then
     (
	  if not R.?sFunction then R.sFunction = new MutableHashTable;
	  if opts.EorH == "E" then
	  (
     	       -----sFunction#0 records s-polynomials in terms of the e-variables
	       if not R.sFunction#?0 then R.sFunction#0 = new MutableHashTable;
	       auxEH = 0;
	       )
	  else
	  (
     	       -----sFunction#1 records s-polynomials in terms of the h-variables
	       if not R.sFunction#?1 then R.sFunction#1 = new MutableHashTable;
	       auxEH = 1;
	       );
     	  auxR = R;
     	  auxn = R.dim;
     	  rez = jT(lam);
	  )
     else
     (
     	  n := #lam;
     	  rez = det(map(R^n, n, (i,j) -> 
	       (
	       	    aux := lam#i-i+j;
	       	    if aux < 0 or aux>R.dim then 0_R
	       	    else if aux == 0 then 1_R else u aux)
	       ),
	  Strategy => Cofactor);
	  );
     rez
     )

--computes the Jacobi-Trudi determinant recursively
jT = (lambda) ->
(
     lambda = toList lambda;
     rez := local rez;
     if auxR.sFunction#auxEH#?lambda then rez = auxR.sFunction#auxEH#lambda
     else
     (
     ll := #lambda;
     if ll == 0 or lambda#0 == 0 then rez = 1_auxR else
     if ll == 1 then rez = auxR_(2*auxEH*auxn-1+lambda#0) else
     (
	  l1 := drop(lambda,-1);
     	  l2 := {};
	  rez = 0;
	  sgn := 1;
	  for i from 0 to ll-1 do
	  (
     	       if lambda#(ll-1-i)+i<=auxn then --just added, won't work for h-polynomials
	       rez = rez + sgn*auxR_(2*auxEH*auxn-1+lambda#(ll-1-i)+i)*jT(l1|l2);
	       sgn = - sgn;
	       l1 = drop(l1,-1);
	       if lambda#(ll-1-i)>1 then
	       l2 = {lambda#(ll-1-i)-1} | l2;
	       );
	  );
     auxR.sFunction#auxEH#lambda = rez;
     );
     rez
     )
---------------------------------------------------------------
--------------End Jacobi-Trudi---------------------------------
---------------------------------------------------------------


---------------------------------------------------------------
--------------Plethysm-----------------------------------------
---------------------------------------------------------------

--the cycle type of the k-th power of any permutation of cycle type cyc
powerCycleType := method()
powerCycleType(ZZ,List) := (k,cyc) ->
(
     rsort(flatten (for i in cyc list (g := gcd(i,k);splice{g:i//g})))
     )

-- d is an integer
-- R is symmetricRing n
-- returns the plethysm map p_d : R --> R
--    which sends p_i to p_(i*d).
plethysmMap = (d,maxg,R) -> (
     nS := R.dim;
     nSd := nS // d;
     fs := splice{nS:0_R};
     topf := min(maxg,nSd);
     fs = join(fs, apply(1..topf, j -> R.pVariable(d*j)));
     if maxg > nSd then 
        fs = join(fs, apply(topf+1..maxg,j-> R.mapFromE R.PtoETable#(d*j)));
     fs = join(fs, 2*nS-maxg:0_R);
     map(R,R,fs)
     )

-- exterior plethysm (corresponding to composition
-- of Schur functors of GL-representations)
-- f is a polynomial in symmetricRing / SchurRing SA
-- g is a polynomial in symmetricRing / SchurRing SB
-- result is in symmetricRing / SchurRing SB
plethysmGL = method()
plethysmGL(RingElement,RingElement) := (f,g) -> (
     Rg := ring g;
     Rf := ring f;
     if schurLevel Rf > 1 then error"Undefined plethysm operation";

     issy := not instance(Rg,SchurRing);
     pg := toP g;
     pf := toP f;
     
     SRg := ring pg; --symmetric ring of Rg
     SRf := ring pf; --symmetric ring of Rf
          
     nf := SRf.dim;
--maxf is the maximum i for which the variable p_i appears in the expression of pf
     maxf := max(support(pf)/index//max-nf+1,0);
     
     auxS := SRg;
     nS := auxS.dim;
     lev := schurLevel auxS;
     spg := support(pg)/index;
--maxg is the maximum i for which the variable p_i appears in the expression of pg
     maxg := max(select(spg,i->i<3*nS)//max-nS+1,0);
--if p_(maxf*maxg) hasn't been computed in terms of E-polynomial, then compute it
     if maxf*maxg >= #auxS.PtoETable then PtoE(maxf*maxg,auxS);
--phi is the map that sends p_i to the plethystic composition p_i\circ pg
--so that phi(f) = f \circ pg (plethysm of f and pg)
     phi := map(SRg,SRf,flatten splice {nf:0_SRg,
	       apply(1..nf, j -> (if j<=maxf then (plethysmMap(j,maxg,SRg))pg else 0_SRg)),
	       nf:0_SRg});
     pl := phi pf;
     if issy then pl else toS pl
)


-- interior plethysm (corresponding to the result of
-- the application of a Schur functor to an S_n-representation)
-- f is a polynomial in symmetricRing N / SchurRing SA
-- g is a polynomial in symmetricRing n / SchurRing SB
-- result is in symmetricRing n / SchurRing SB
plethysmSn = method()
plethysmSn(RingElement,RingElement) := (f,g) ->
(
     symmetricFunction(plethysm(f,classFunction g), ring g)
     )

-- plethysm of symmetric functions
plethysm = method()

-- this function is not exported
-- it is used to compute the plethysm of f and g
-- when f is a power-sum symmetric polynomial
auxplet = method()
auxplet(RingElement,RingElement) := (f,g) ->
(
     Rg := ring g;
     pl := local pl;
     if Rg.GroupActing == "GL" then pl = plethysmGL else
     if Rg.GroupActing == "Sn" then pl = plethysmSn;
     sLg := schurLevel Rg;
     
     if sLg == 1 then return pl(f,g) else
     (
     	  lF := listForm g;
	  return sum for t in lF list auxplet(f,last t) * pl(f,Rg_(first t))
	  );
     )

-- the most general form of plethysm
-- f is an arbitrary symmetric functions
-- g is an element of a representation ring of a product of general linear and/or symmetric groups
plethysm(RingElement,RingElement) := (f,g) ->
(
     pf := toP f;
     Rf := ring pf;
     if schurLevel Rf > 1 then error"Undefined plethysm operation";
     
     pls := new MutableHashTable from {};
     lpf := listForm pf;
     m := (ring pf).dim;
     isSchur := instance(ring g,SchurRing);

     auxg := local auxg;
     if isSchur then auxg = g else auxg = toS g;

     pl := sum for t in lpf list ((last t) * product select(apply(splice{0..m-1}, i -> (ex := (first t)#(m+i);
     	       if ex > 0 then (if pls#?i then (pls#i)^ex else 
	        (pls#i = auxplet(Rf.pVariable(i+1),auxg);(pls#i)^ex)))),j -> j =!= null)); -- this is bad when g is not in a SchurRing

     if isSchur then pl else toSymm pl
     )

-- plethysm of s_lambda and g
plethysm(BasicList,RingElement) := (lambda,g) -> (
     d := sum toList lambda;
     Rf := symmetricRing(QQ,d);
     f := jacobiTrudi(lambda,Rf);
     plethysm(f,g)
     )

-- (inner) plethysm of symmetric function f with the class function cF (the character of a certain S_n-representation)
plethysm(RingElement,ClassFunction) := (f,cF) ->
(
     R := ring(cF#(first keys cF));
     if R === ZZ then R = QQ;

     pf := toP f;
     n := degree cF;
     k := (ring pf).dim;
     pvars := (ring pf).pVariable;
     parsn := toList \ partitions(n);  
     newHT := new MutableHashTable;
     for sig in parsn do
     (
	  sublist := for i from 1 to k list
	  (
	       pct := powerCycleType(i,sig);     
	       if cF#?pct then cF#pct else 0
	       );
	  newHT#sig = (map(R,ring pf,splice{k:0} | sublist | splice{k:0})) pf;
	  );
     new ClassFunction from newHT
     )

-- (inner) plethysm of s_lambda with the class function cF (the character of a certain S_n-representation)
plethysm(BasicList,ClassFunction) := (lambda,cF) -> (
     d := sum toList lambda;
     Rf := symmetricRing(QQ,d);
     f := jacobiTrudi(lambda,Rf);
     plethysm(f,cF))

-*
-- degree of a polynomial in a SchurRing
-- this is no longer used
degSchurPol = method()
degSchurPol(RingElement) := ps -> (
     tms := listForm ps;
     tms/first/sum//max
     )
*-
---------------------------------------------------------------
-----------End plethysm----------------------------------------
---------------------------------------------------------------


---------------------------------------------------------------
----Transition between various types of symmetric functions----
---------------------------------------------------------------

-- toSymm 
toSymm = method()

-- if ps is an element of a schurRing R
-- toSymm returns the symmetric function corresponding to ps, as an element of a symmetricRing, the symmetricRing R;
-- otherwise ps is returned;
toSymm(RingElement) := (ps) ->
(
     S := ring ps;
     if instance(S, SchurRing) then
     (
     R := symmetricRing S;
     tms := listForm ps;
--each term s_lambda in ps is transformed into an element of R using the jacobiTrudi routine
     sum apply(tms,(p,a)->(
	       (try b:=jacobiTrudi(p,R) then b else error"Need symmetric ring of higher dimension")*
	       toSymm(lift(a,coefficientRing S))))
     )
     else return ps
)

-- this is the base case of the recursive operation in the general case
-- needed when ps is an element of ZZ or QQ, because ZZ, QQ don't have
-- RingElement as an ancestor
toSymm(Number) := (ps) -> ps

mapSymToE = method()
-- writes the symmetric functions of maximal schurLevel in f (i.e. those
-- not contained in the coefficient ring of R) in terms of the e-polynomials
mapSymToE (RingElement) := (f) -> (
     R:=ring f; 
     if R.?mapSymToE then R.mapSymToE f else f
)
mapSymToH = method()
-- writes the symmetric functions of maximal schurLevel in f (i.e. those
-- not contained in the coefficient ring of R) in terms of the h-polynomials
mapSymToH (RingElement) := (f) -> (
     R:=ring f; 
     if R.?mapSymToH then R.mapSymToH f else f
)
mapSymToP = method()
-- writes the symmetric functions of maximal schurLevel in f (i.e. those
-- not contained in the coefficient ring of R) in terms of the p-polynomials
mapSymToP (RingElement) := (f) -> (
     R:=ring f; 
     if R.?mapSymToP then R.mapSymToP f else f
)

toE = method()
-- writes a symmetric function (possibly in a ring
-- with schurLevel larger than one) in terms of
-- elementary symmetric polynomials
toE (RingElement) := (f) -> (
     R := ring f;
     if class R === SchurRing then toE toSymm f
     else 
     (
	  if not R.?schurLevel then f else
	  if R.schurLevel>1 then terms f/(i->(toE leadCoefficient i*(mapSymToE leadMonomial i)))//sum
	  else mapSymToE f
	  )
     )

toP = method()
-- writes a symmetric function (possibly in a ring
-- with schurLevel larger than one) in terms of
-- power sums
toP (RingElement) := (f) -> (
     R := ring f;
     if class R === SchurRing then toP toSymm f
     else 
     (
	  if not R.?schurLevel then f else
	  if R.schurLevel>1 then terms f/(i->(toP leadCoefficient i*(mapSymToP leadMonomial i)))//sum
	  else mapSymToP f
	  )
     )

toH = method()
-- writes a symmetric function (possibly in a ring
-- with schurLevel larger than one) in terms of
-- complete symmetric polynomials
toH (RingElement) := (f) -> (
     R := ring f;
     if class R === SchurRing then toH toSymm f
     else 
     (
	  if not R.?schurLevel then f else
	  if R.schurLevel>1 then terms f/(i->(toH leadCoefficient i*(mapSymToH leadMonomial i)))//sum
	  else mapSymToH f
	  )
     )

-- auxiliary functions to be used in
-- the recTrans routine
leadTermFcn := local leadTermFcn;
retFcn := local retFcn;
mappingFcn := local mappingFcn;

toS = method()

toS(RingElement) := (f) -> (
     R := ring f;
     if (schurLevel R == 0 or class R === SchurRing) then f else
     (
	  S := schurRing R;
     	  local hf;
     	  n := R.dim;
     	  d := first degree f;
     	  ngS := numgens S;
--mappingFcn v is used when v = h_i for some i; it returns the Schur polynomial s_i, in the correct Schur ring
     	  mappingFcn = (v) -> (schurRing ring v)_{index v-2*(ring v).dim+1};
--leadTermFcn takes as input a polynomial pl in the h-variables, 
--and returns the variable h_i, with i maximal, such that h_i appears in the expression of pl 
     	  leadTermFcn = (pl) -> (
     	       R := ring pl;
     	       spl := select(support pl,i->index i<numgens R);
     	       if spl == {} then null else last spl
     	       );
--retFcn takes as input a polynomial that's liftable to its coefficient ring, it lifts it and expresses it in the s-basis
--this is used when dealing with a symmetricRing A of positive schurLevel, and pl appears as a coefficient of some
--h-polynomial in A, so it is liftable to the coefficient ring of A
     	  retFcn = (pl) -> toS lift(pl,(coefficientRing ring pl));
     	  promote(recTrans(toH f),S)
	  )
     )

toS(Thing) := (f) -> f
undocumented(toS,Thing)

toS(Thing,Ring) := (f,T) -> try(lift(f,T)) else f
undocumented(toS,Thing,Ring)

toS(RingElement,SchurRing) := (f, T) ->
(
     R := ring f;
     if schurLevel R == 0 then 
     (
	  U := T;
	  while schurLevel U > 0 do U = coefficientRing U;
	  toS(f,U)
	  )
     else
     (
     	  fS := toS f;
     	  dimT := numgens T;
     	  (listForm fS)/(i-> if #i#0<=dimT then T_(i#0)*toS(i#1,coefficientRing T) else 0_T)//sum
	  )
     )

--recTrans is a recursive routine that transforms an h-polynomial (in a symmetricRing of positive schurLevel)
--into an s-polynomial, by proceeding one level at a time
recTrans = method()
recTrans (RingElement) := (pl) ->
(
--lead = leading variable = h_i with i maximal
     lead := leadTermFcn pl;
     isSn := (ring pl).GroupActing == "Sn";
     if lead === null then retFcn pl else
     (
--monomials/coefficients with respect to the leading variable lead
	  (mon,coe) := coefficients(pl,Variables=>{lead});
	  mon = flatten entries mon;
	  coe = flatten entries coe;
     	  rez := 0;
	  cdeg := degree(lead,mon#0)+1;
	  for i from 0 to #mon-1 do
	  (
	       fdeg := degree(lead,mon#i);
	       while (cdeg>fdeg+1) do
	       (
		    cdeg = cdeg - 1;
--if the group acting at a given level is the symmetric group
--use internal multiplication of symmetric functions
--otherwise use usual multiplication
		    if isSn then rez = rez**mappingFcn(lead) else
		    	 rez = rez*mappingFcn(lead);
		    );
	       if isSn then rez = rez**mappingFcn(lead)+recTrans(coe#i)
	       else rez = rez*mappingFcn(lead)+recTrans(coe#i);
	       cdeg = cdeg - 1;
	       );
	  while cdeg>0 do 
	       (
		    cdeg = cdeg - 1;
--if the group acting at a given level is the symmetric group
--use internal multiplication of symmetric functions
--otherwise use usual multiplication
		    if isSn then rez = rez**mappingFcn(lead) else
		    	 rez = rez*mappingFcn(lead);
		    );
	  rez
     	  )
     )

recTrans(Thing) := p -> p

--------
--------
--given a recursive relation for a sequence a_n, given by a convolution of (a_n) with (L_n)
--convolve computes formulas for a_n in terms of L_n
--the main routine is coded in the engine
--the value of conv is used to indicate one of several types of convolution
convolve = method()
convolve(List,ZZ) := (L,conv) -> (
     A := ring L_0;
     toList drop(apply(rawConvolve(L/raw//toSequence, conv), f -> new A from f),1)
     )

--a_n = p_n
--L_n = e_n
PtoE = (m,R) -> (
     n := R.dim;
     A := R.symRingForE;
     p2e := prepend(1_A, for i from 1 to n list ((-1)^(i+1) * A_(2*n+i-1)));
     if m>n then p2e = join(p2e,toList((m-n):0_A));
     R.PtoETable = if n == 0 then {1_A} else {1_A} | (- convolve(p2e,2));
     )

--a_n = h_n
--L_n = e_n
HtoE = (m,R) -> (
     n := R.dim;
     A := R.symRingForE;
     h2e := prepend(1_A, for i from 1 to n list (-1)^(i+1)*A_(2*n+i-1));
     R.HtoETable = if n == 0 then {1_A} else {1_A} | convolve(h2e,0);
     )

--a_n = h_n
--L_n = p_n
HtoP = (m,R) -> (
     n := R.dim;
     A := R.symRingForP;
     h2p := prepend(1_A, for i from 1 to n list A_(2*n+i-1));
     R.HtoPTable = if n == 0 then {1_A} else {1_A} | convolve(h2p,1);
     )

--a_n = e_n
--L_n = p_n
EtoP = (m,R) -> (
     n := R.dim;
     A := R.symRingForP;
     e2p := prepend(1_A, for i from 1 to n list (-1)^(i+1)*A_(2*n+i-1));
     R.EtoPTable = if n == 0 then {1_A} else {1_A} | convolve(e2p,1);
     )

--a_n = p_n
--L_n = h_n
PtoH = (m,R) -> (
     n := R.dim;
     A := R;
     p2h := prepend(1_A, for i from 1 to n list (- A_(2*n+i-1)));
     R.PtoHTable = if n == 0 then {1_A} else {1_A} | convolve(p2h,2);
     )

--a_n = e_n
--L_n = h_n
EtoH = (m,R) -> (
     n := R.dim;
     A := R;
     e2h := prepend(1_A, for i from 1 to n list (-1)^(i+1)*A_(2*n+i-1));
     R.EtoHTable = if n == 0 then {1_A} else {1_A} | convolve(e2h,0);
     )


---------------------------------------------------------------
--------------End transition-----------------------------------
---------------------------------------------------------------

---------------------------------------------------------------
-------------Schur Resolutions---------------------------------
---------------------------------------------------------------

--recsyz is a recursive method that takes as input an element el of a SchurRing of positive schurLevel
--and returns the sum of the terms having negative coefficients
--it is used in the routine schurRes to determine representations that are forced to be generators
--of syzygy modules in an equivariant resolution
recsyz = method()
recsyz (Thing) := (el) -> min(el,0)
recsyz (RingElement) := (el) ->
(
     T := ring el;
     listForm el/((u,v)->T_u*recsyz(v))//sum
     )

schurResolution = method(Options => {DegreeLimit => 0, SyzygyLimit => 0})
schurResolution(RingElement,List) := opts -> (rep,M) ->
(
     d := opts.DegreeLimit;
     if d == 0 then d = #M-1;
     c := opts.SyzygyLimit;

     T := ring rep;
     n := schurLevel T;
--plets is the list of symmetric powers of the representation rep, from 0 to d
     plets := new MutableList;
     plets#0 = 1_T;
     for i from 1 to d do plets#i = symmetricPower(i,rep);

     schurRes(rep,M,new List from plets,DegreeLimit => d,SyzygyLimit => c)
     )

schurResolution(RingElement,List,List) := opts -> (rep,M,plets) ->
(
     d := opts.DegreeLimit;
     if d == 0 then d = #M-1;
     c := opts.SyzygyLimit;
     
     schurRes(rep,M,plets,DegreeLimit => d,SyzygyLimit => c)
     )

schurRes = method(Options => options schurResolution)
schurRes(RingElement,List,List) := opts -> (rep,M,plets) ->
(
     T := ring rep;
     d := opts.DegreeLimit;
     c := opts.SyzygyLimit;
     
     mods := new MutableList from (M | toList((d+1-#M):0));
     notdone := true;
     k := 0;
--syzy is the list of the characters of the generators of the syzygy modules
     syzy := new MutableList;
     syzy#k = {};
     local mo;
     local newsyz;
     
--syzygy modules are constructed step by step
--the stopping condition is either reaching the limit c of syzygy modules that are computed
--or not finding any new syzygies at a given step
     while notdone do
     (
	  for i from 0 to d do
	  (
     	       mo = 0_T;	       
	       for sy in syzy#k do
	       	    if sy#0 <= i then mo = mo + plets#(i-sy#0) * sy#1
		    else break;
--mods is a sequence of representations, mods#i being the degree i of a module that needs to be ``covered''
--by the differential in the equivariant complex
--mo is the degree i part of the new syzygy module
--it needs to ``cover'' mods#i, i.e. there has to exist a surjective map of representations from
--mo to mods#i
	       mo = mo - mods#i;
--if there are representations with negative coefficients in mo-mods#i, it means that mo doesn't cover mods#i
--the representations with negative signs must be ``covered'' by new syzygies
	       newsyz = recsyz(mo);
	       if newsyz != 0 then syzy#k = syzy#k | {(i,-newsyz)};
	       mods#i = mo - newsyz;
	       );
     	  if c == 0 then notdone = not (syzy#k == {})
	  else notdone = (k<c);
     	  k = k + 1;
	  syzy#k = {};
 	  );
     select(toList syzy,i-> i != {})
     )
					      
---------------------------------------------------------------
-------------end Schur Resolutions-----------------------------
---------------------------------------------------------------


---------------------------------------------------------------
--------------Characters of Symmetric Group--------------------
---------------------------------------------------------------

--given a partition lambda as a nonincreasing sequence of positive integers
--seqToMults returns the representation of this partition as a sequence
--of multiplicities: rez#i is the number of parts of lambda of size (i+1)
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

--given a partition lambda represented in as a sequence of multiplicities mults
--where mults#i is the number of parts of lambda of size (i+1)
--multsToSeq represents lambda as a nonincreasing sequence of positive integers
multsToSeq = method()
multsToSeq(List) := (mults) ->
(
     n := #mults;
     par := {};
     for i from 0 to n-1 do
         par = par | splice{mults#i:(i+1)};
     reverse par
     )

--the size of the centralizer of a permutation of cycle type lambda
centralizerSize = method()
centralizerSize(List) := lambda ->
(
     product for i from 0 to #lambda-1 list((i+1)^(lambda#i)*(lambda#i)!)
     )

keysCF := method()
keysCF(ClassFunction) := (cF) -> keys cF

degree(ClassFunction) := ch ->
(
     ke := keysCF ch;
     if #ke == 0 then -1 else sum(first ke)
     )

--go from symmetric functions to class functions
classFunction = method()
classFunction(RingElement) := (f)->
(
     Rf := ring f;

     R := symmetricRing Rf;
     pf := toP f;
     n := R.dim;
     
     if (degree pf)#0 > n then error"Can't interpret ring element as a symmetric function";
     
     (mon,coe) := apply(coefficients pf,i->flatten entries i);
     ch := new MutableHashTable;
     for j from 0 to #mon-1 do
     (
     	  degs := (flatten exponents mon#j)_{(n)..(2*n-1)};
     	  par := multsToSeq(degs);
	  ch#par = lift(coe#j,coefficientRing R) * centralizerSize(degs);
	  );
     new ClassFunction from ch
     )

classFunction(BasicList) := (lambda)->
(
     lam := toList(lambda);
     s := symbol s;
     R := schurRing(QQ,s,sum lam);
     classFunction(R_lam)    
     )

ClassFunction + ClassFunction := (ch1,ch2)->
(
     clSum := new MutableHashTable;
     l1 := sum((keysCF ch1)#0);
     l2 := sum((keysCF ch2)#0);
     if l1 != l2 then error("The symmetric functions/characters must have the same degree");
     for i in unique(keysCF(ch1)|keysCF(ch2)) do
     	  (
	       a := b := 0;
	       if ch1#?i then a = ch1#i;
	       if ch2#?i then b = ch2#i;
	       if (a+b != 0) then clSum#i = a + b;
	       );
     new ClassFunction from clSum
     )

RingElement * ClassFunction := Number * ClassFunction := (n,ch) ->
(
     clProd := new MutableHashTable;
     for i in keysCF ch do clProd#i = n*ch#i;
     new ClassFunction from clProd     
     )

ClassFunction * RingElement := ClassFunction * Number := (ch,n) -> n*ch;


ClassFunction - ClassFunction := (ch1,ch2)-> ch1 + (-1)*ch2;

ClassFunction == ClassFunction := (ch1,ch2) ->
(
     equ := true;
     for i in unique(keysCF ch1 | keysCF ch2) do
     	  if not ((not ch1#?i and not ch2#?i) or (ch1#?i and ch2#?i and ch1#i == ch2#i)) then 
     	  (
	       equ = false;
	       break;
	       );
     equ
     )

--go from class functions to symmetric functions
symmetricFunction = method()
symmetricFunction(ClassFunction,Ring) := (ch,S)->
(
     R := symmetricRing S;
     rez := 0_R;
     n := R.dim;
     for lam in keysCF ch do
     	  rez = rez + ch#lam * (product for i from 0 to #lam-1 list R.pVariable(lam#i)) / centralizerSize(seqToMults lam);
     if instance(S, SchurRing) then toS rez else rez
     )

scalarProduct = method()
scalarProduct(ClassFunction,ClassFunction) := (ch1,ch2)->
(
     scProd := 0;
     chProd := internalProduct(ch1,ch2);
     for i in keysCF(chProd) do
     	  scProd = scProd + chProd#i / centralizerSize(seqToMults i);
     scProd
     )

scalarProduct(RingElement,RingElement) := (f1,f2)->
(
     ch1 := classFunction f1;
     ch2 := classFunction f2;
     scalarProduct(ch1,ch2)
     )

internalProduct = method()
ClassFunction * ClassFunction := 
internalProduct(ClassFunction,ClassFunction) := (ch1,ch2)->
(
     iProd := new MutableHashTable;
     l1 := sum((keysCF ch1)#0);
     l2 := sum((keysCF ch2)#0);
     if l1 == 0 then return(ch1#{} * ch2);
     if l2 == 0 then return(ch2#{} * ch1);
     if l1 != l2 then error("The symmetric functions/characters must have the same degree");
     for i in keysCF(ch1) do
     	  if ch2#?i then iProd#i = ch1#i * ch2#i;
     new ClassFunction from iProd
     )

internalProduct(RingElement,RingElement) := (f1,f2)->
(
     R2 := ring f2;
     R := local R;
     issy := false;
     if (class R2 =!= SchurRing) then issy = true;
     R = symmetricRing ring f2;
     ch1 := classFunction f1;
     ch2 := classFunction f2;
     rez := symmetricFunction(internalProduct(ch1,ch2),R);
     if issy then rez else
     toS rez
     )

-*
chi(BasicList,BasicList) := (lambda, rho) ->
(
     la := toList lambda;
     rh := toList rho;
     ll := sum la;
     if ll != sum(rh) then error"Partitions must have the same size.";
     R := symmetricRing(QQ,ll);
     sl := jacobiTrudi(la,R);
     pr := 1_R;
     for i from 0 to #rh-1 do pr = pr * R_(ll-1+rh#i);
     scalarProduct(sl,pr)
     )
*-
---------------------------------------------------------------
--------------End characters-----------------------------------
---------------------------------------------------------------

--------------------------------
-- Dimension -------------------
--------------------------------
-- Function to compute the dimension of a virtual representation

hooklengths = (lambda) -> (
     mu := conjugate lambda;
     product for i from 0 to #lambda-1 list (
	  product for j from 0 to lambda#i-1 list (
	       lambda#i + mu#j - i - j -1
	       ))
     )

dimSchur = method(Options => {GroupActing => "GL"})
dimSchur(Thing,List) := opts -> (n, lambda) -> (
     -- lambda is a list {a0,a1,...,a(r-1)}, a0 >= a1 >= ... >= a(r-1) > 0
     -- n can be a number or a symbol
     powers := new MutableList from toList((if lambda#?0 then lambda#0 else 0) + #lambda - 1 : 0);
     base := 1 - #lambda;
     for i from 0 to #lambda-1 do
       for j from 0 to lambda#i-1 do
       	    powers#(j-i-base) = powers#(j-i-base) + 1;
     if not instance(n,ZZ) then n = hold n;
     -- now get the hook lengths
     answer := local answer;
     if opts.GroupActing == "GL" then
     (
	  num := product for s from 0 to #powers-1 list (n + (base+s))^(powers#s);
     	  answer = num/hooklengths(new Partition from lambda);
	  )
     else if opts.GroupActing == "Sn" then answer = (sum toList lambda)! / hooklengths(new Partition from lambda);
     answer
     )
dimSchur(Thing,SchurRingElement) := opts -> (n, F) -> (
     -- assumption: F is an element in a SchurRing of level 1
     if schurLevel(ring F) != 1 then error"Expected a list as input";
     L := listForm F;
     sum apply(L, p -> (
     	       lambda := p#0;
	       p#1 * dimSchur(n,lambda,opts)))
     )

dimSchur(List,SchurRingElement) := opts -> (lis, F) -> (
     -- assumption: F is an element in a SchurRing
     if #lis != schurLevel(ring F) then error"Input list has incorrect size";
     gr := (ring F).GroupActing;
     L := listForm F;
     sum apply(L, p -> (
     	       lambda := p#0;
	       if instance(p#1,SchurRingElement) then dimSchur(drop(lis,1),p#1) * dimSchur(lis#0,lambda,GroupActing => gr)
                  else p#1 * dimSchur(lis#0,lambda,GroupActing => gr)))
     )

dimSchur(SchurRingElement) := opts -> (F) -> (
     schurdims := (S) -> (
	  if schurLevel S === 0 then {}
	  else prepend(numgens S, schurdims coefficientRing S));
     ns := schurdims ring F;
     if any(ns, i -> not instance(i,ZZ))
     then error "expected finitely generated Schur rings";
     dS := dimSchur(ns,F);
     if liftable(dS,ZZ) then lift(dS,ZZ) else dS
     )
---------------------------------------------------------------
--------End dimension----------------------------------------------
---------------------------------------------------------------


---------------------------------------------------------------
-----------Partitions-related functions------------------------
---------------------------------------------------------------
--this part might have to be moved elsewhere
--since it's not directly connected to the package
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
locind = local locind;
genPartitions = local genPartitions;

genPartitions = method()
genPartitions(ZZ) := (k)->
(
     if k==length locS then (locind = locind + 1;locPartitions#locind = set toList locParts) else
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

partitions(Set,BasicList) := (S,L)->
(
     locS = toList S;
     locL = L;
     locLengthL = #L;
     locParts = new MutableList;
     for i from 0 to locLengthL-1 do locParts#i = set{};
     locPartitions = new MutableList;
     locind = -1;
     genPartitions(0);
     toList locPartitions
     )

--------end generate partitions

---------------------------------------------------------------
--------End partitions-related functions-----------------------
---------------------------------------------------------------

beginDocumentation()

undocumented {Schur}

doc ///
Key
  SchurRings
Headline
  Rings representing irreducible representations of general linear or symmetric groups
Description
  Text
    This package makes computations in the representation rings of general linear groups 
    and symmetric groups possible.
    
    Given a positive integer {\tt n} we may define a polynomial ring in {\tt n}
    variables over an arbitrary base ring , whose monomials correspond to the irreducible 
    representations of {\tt GL(n)}, and where multiplication is given by the decomposition of 
    the tensor product of representations. We create such a ring in Macaulay2 using the 
    @TO schurRing@ function.
  
  Example
    S = schurRing(QQ,s,4)
    R = schurRing(r,infinity)
    
  Text    
    Note that in the above, {\tt n} is allowed to be equal to {\tt \infty}. However, in this
    version of the package, many of the features from the case {\tt n} finite are missing
    from the infinite case, so the user is advised to use large values for {\tt n} as a
    substitute, whenever necessary.

    We determine the relative dimension of a SchurRing over its base using the @TO numgens@ function:

  Example 
    numgens S
    numgens R
   
  Text
    
    For {\tt k\leq n}, one may interpret the degree
    {\tt k} homogeneous component of a @TO SchurRing@ as the representation ring of the symmetric
    group {\tt S_k}. In this ring, the multiplication is different than the one in 
    the representation ring of {\tt GL(n)}. By default, the elements of a @TO SchurRing@ are 
    interpreted as (virtual) characters of 
    a general linear group. This interpretation is controlled by the option @TO GroupActing@,
    whose default value is "GL". To indicate that the elements of a Schur ring should
    be interpreted as characters of the symmetric group, one has to set the option @TO GroupActing@
    to "Sn".
  
  Example
    Q = schurRing(q,4,GroupActing => "Sn")
    
  Text
        
    A monomial in {\tt S} represents the irreducible representation with a given highest weight. 
    The standard {\tt GL(4)}-representation is
   
  Example
    V = s_1

  Text
    
    We may see the dimension of the corresponding irreducible representation using @TO dim@:

  Example
    dim V

  Text
    Multiplication of elements corresponds to tensor product of representations. The 
    value is computed using a variant of the Littlewood-Richardson rule.
  
  Example
    V * V
    V^3

  Text 
    
    The third symmetric power of {\tt V} is obtained by
     
  Example
    W = s_{3}
    dim W
   
  Text
    
    and the third exterior power of {\tt V} can be obtained using

  Example
    U = s_{1,1,1}
    dim U
    
  Text
  
    Alternatively, one can use the functions @TO symmetricPower@ and @TO exteriorPower@:
    
  Example
    W = symmetricPower(3,V)
    U = exteriorPower(3,V)
    
  Text
  
    We can in fact take symmetric powers and exterior powers of any representation:
    
  Example
    exteriorPower(2,W)
    symmetricPower(2,U)

  Text
  
    and compute even more general forms of @TO plethysm@:
    
  Example
     plethysm(W+U,W+U)
   
  Text
    
    Alternatively, we can use the binary operator @TO symbol \@ @ to compute plethysm:
  
  Example
    s_2 @ s_3
    (W+U) @ (W+U)

  Text
  
    All the above calculations assume that we're dealing with representations of {\tt GL(4)}.
    But as symmetric functions of degree three, {\tt W} and {\tt U}, can be thought of as characters of the
    symmetric group {\tt S_3}. Let us first ``move'' these symmetric functions into a Schur ring
    designed to deal with characters of symmetric groups (like the ring {\tt Q} defined
    above):
    
  Example
    W' = toS(W,Q)
    U' = toS(U,Q)
    
  Text
    
    Now {\tt W'} corresponds to the trivial representation of {\tt S_3},
    and {\tt U'} to the sign representation. As such, we can tensor them together using the
    function @TO internalProduct@, or the binary operator @TO symbol *@.
    
  Example
    W' * U'

  Text
  
    We can generate the class function corresponding to an {\tt S_n}-representation, using
    the function @TO classFunction@:
    
  Example
    cfW = classFunction(W')
    cfU = classFunction(U')
    
  Text
    
    We can multiply class functions together, and transform class functions into symmetric
    functions using the function @TO symmetricFunction@:
    
  Example
    cfWU = cfW * cfU
    symmetricFunction(cfWU,Q)
    
  Text
  
    The result of the previous computation is of course the same as that of taking the product
    of {\tt W'} and {\tt U'}.
    
    We can take exterior and symmetric powers of {\tt S_n}-representations, just as for
    {\tt GL}-modules (compare to {\tt o16} and {\tt o17}):
    
  Example
    exteriorPower(2,W')
    symmetricPower(2,U')

  Text
      
    We can write any symmetric function in terms of the standard {\tt e}- (elementary
    symmetric), {\tt h}- (complete) and {\tt p}- (power sum) bases, using the functions 
    @TO toE@, @TO toH@, @TO toP@ respectively:
    
  Example
    toE U
    toH U
    toP W
    
  Text
    
    These expressions live in the Symmetric ring associated to {\tt S}, which can be obtained
    using the function @TO symmetricRing@:
    
  Example
    A = symmetricRing S
    
  Text
  
    Similarly, any Symmetric ring has a Schur ring attached to it, which can be obtained using
    the function @TO schurRing@:
    
  Example
    schurRing A === S
  
  Text  
  
    We construct tensor products of Schur rings iteratively by allowing Schur rings over
    base rings that are also Schur rings:
    
  Example
    T = schurRing(S,t,3)
    
  Text
    
    The Schur ring {\tt T} can thus be thought of as the representation ring of 
    {\tt GL(V)\times GL(V')}, where {\tt V} is as before a vector space of dimension 
    {\tt 4}, and {\tt V'} is a vector space of dimension {\tt 3}. The representation 
    corresponding to {\tt V'} is
  
  Example
    V' = t_1
  
  Text
   
    The function @TO schurLevel@ indicates the number of Schur rings that have been
    tensored together to obtain any given ring:
    
  Example
    schurLevel T
    schurLevel S
    schurLevel QQ
    
  Text
    
    We can now check Cauchy's formula for decomposing symmetric/exterior powers of a
    tensor product:
    
  Example
    symmetricPower(3,V*V')
    exteriorPower(3,V*V')
  
  Text
  
    We end with the computation of the {\tt GL(n)}- and {\tt S_n}-equivariant resolutions
    of the residue field of a polynomial ring in {\tt n} variables. The function that does
    this calculation, @TO schurResolution@, is based on an empirical method, which gives
    the correct answer in surprisingly many situations.
    
    In the {\tt GL(n)} situation, we are resolving the residue field which as a representation
    has character {\tt 1_S}. The space of linear forms in the polynomial ring 
    considered as a {\tt GL}-representation has character {\tt V = s_1}.
    
  Example
    n = 4
    M = {1_S}
    schurResolution(V,M,DegreeLimit => n)
    
  Text
  
    Not surprisingly, the syzygy modules are generated by the exterior powers of {\tt V}.

    The residue field as a representation of the symmetric group {\tt S_n}
    has character {\tt s_n}. The space of linear forms in the polynomial ring 
    considered as an {\tt S_n}-representation coincides with the permutation representation
    of {\tt S_n}, thus has character {\tt s_n + s_{n-1,1}}.

  Example
    rep = q_n + q_(n-1,1)
    M = {q_n}
    sR = schurResolution(rep,M,DegreeLimit => n)
 
  Text
    
    We can check that the second syzygy module is generated by the second exterior power of the permutation
    representation.
    
  Example
    eP2rep = exteriorPower(2,rep)
    eP2rep == last sR#2#0
///

doc ///
Key
  SchurRing
  (symbol _,SchurRing,List)
  (symbol _,SchurRing,Sequence)
  (symbol _,SchurRing,ZZ)
Headline
  The class of all Schur rings
Description
  Text
    A Schur ring is the representation ring for the general linear group of {\tt n\times n}
    matrices, and one can be constructed with @TO schurRing@.
  
  Example
    S = schurRing(QQ,s,4)
  
  Text
  
    Alternatively, its elements can be interpreted as virtual characters of symmetric groups,
    by setting the value of the option @TO GroupActing@ to {\tt "Sn"}.
    
  Example
    Q = schurRing(QQ,q,4,GroupActing => "Sn")
  Text
  
    The element corresponding to the Young diagram {\tt \{3,2,1\}}, is obtained as follows.
   
  Example
    s_{3,2,1}

  Text
  
    Alternatively, we can use a @TO Sequence@ instead of a @TO List@ as the index of a Schur
    function.

  Example
    s_(3,2,1)

  Text
  
    For Young diagrams with only one row one can use positive integers as subscripts.
    
  Example
    q_4
     
  Text  
    
    The name of the Schur ring can be used with a subscript to describe a symmetric 
    function.
  
  Example
    Q_{2,2}
    S_5
  
  Text
  
    The dimension of the underlying virtual {\tt GL}-representation can be obtained
    with @TO dim@.
  
  Example
    dim s_{3,2,1}
    
  Text
  
    Multiplication in the ring comes from tensor product of representations.
  
  Example
    s_{3,2,1} * s_{1,1}
    q_{2,1} * q_{2,1}

  Text
  
    To extract data in an element in a SchurRing, use @TO "listForm"@:
  
  Example
    listForm (s_{3})^2
    q_{2,1} * q_{2,1}
    listForm oo
    
SeeAlso
  schurRing
///

doc ///
Key
  schurRing
  (schurRing,Ring,Symbol,ZZ)
  (schurRing,Ring,Thing,ZZ)
  (schurRing,Ring,Symbol)
  (schurRing,Ring,Thing)
  (schurRing,Thing,ZZ)
  (schurRing,Thing)
Headline
  Make a SchurRing
Description
  Text
    {\tt S = schurRing(A,s,n)} creates a Schur ring of degree {\tt n} over the base ring
    {\tt A}, with variables based on the symbol {\tt s}. This is the representation ring
    for the general linear group of {\tt n} by {\tt n} matrices, tensored with the ring
    {\tt A}. If {\tt s} is already assigned a value as a variable in a ring, its base 
    symbol will be used, if it is possible to determine.
   
  Example
    S = schurRing(QQ[x],s,3);
    (x*s_{2,1}+s_3)^2
    
  Text
    Alternatively, the elements of a Schur ring may be interpreted as characters of
    symmetric groups. To indicate this interpretation, one has to set the value of the option 
    @TO GroupActing@ to "Sn".
    
  Example
    S = schurRing(s,4,GroupActing => "Sn");
    exteriorPower(2,s_(3,1))
      
  Text
    If the dimension {\tt n} is not specified, then one should think of {\tt S} as the
    full ring of symmetric functions over the base {\tt A}, i.e. there is no restriction
    on the number of parts of the partitions indexing the generators of {\tt S}.
    
  Example
    S = schurRing(ZZ/5,t)
    (t_(2,1)-t_3)^2

  Text
    If the base ring {\tt A} is not specified, then @TO QQ@ is used instead.

  Example
    S = schurRing(r,2,EHPVariables => (re,rh,rp))
    toH r_(2,1)
     
SeeAlso
  SchurRing
  symmetricRing
///

doc ///
Key
  (coefficientRing, SchurRing)
Headline
  Coefficient ring of a Schur ring
Usage
  coefficientRing S
Inputs
  S:SchurRing
Description
  Text
    Given a Schur ring {\tt S}, the function returns its coefficient ring.
  
  Example
    S = schurRing(ZZ[x],s,4);
    coefficientRing S
    A = schurRing(QQ,a,3);
    B = schurRing(A,b,2);
    coefficientRing B
///

document {
     Key => {SchurRingIndexedVariableTable,(symbol _,SchurRingIndexedVariableTable,Thing)},
     "This class is used as part of the implementation of a type of indexed variable used just for Schur rings.",
     SeeAlso => { IndexedVariableTable }
     }

doc ///
Key
  symmetricRing
  (symmetricRing,Ring,ZZ)
  (symmetricRing,ZZ)
Headline
  Make a Symmetric ring
Usage
  symmetricRing(A,n)
  symmetricRing n
Inputs
  A:Ring
  n:ZZ
Description
  Text

    The method {\tt symmetricRing} creates a Symmetric ring of dimension {\tt n} over a base ring
    {\tt A}. This is the subring of the ring of symmetric functions over the base {\tt A}
    consisting of polynomials in the first {\tt n} elementary (or complete, or power sum)
    symmetric functions. If {\tt A} is not specified, then it is assumed to be @TO QQ@.
  
  Example
    R = symmetricRing(QQ[x,y,z],4)
    e_2*x+y*p_3+h_2
    toS oo

  Text
    
    The elements of a Symmetric ring can be interpreted as characters of either symmetric or
    general linear groups. This is controlled by the value of the option @TO GroupActing@, whose
    default value is "GL" (general linear group). The other possibility for its value is 
    "Sn" (symmetric group).
    
  Example
    R = symmetricRing(QQ,3,GroupActing => "Sn")
    toE symmetricPower(2,e_2)
    
SeeAlso
  SchurRing
///

doc ///
Key
  eVariable
Headline
  Elementary symmetric functions in a Symmetric ring
Description
  Text
    For a Symmetric ring {\tt R} of dimension {\tt n}, {\tt R.eVariable} is a function 
    which assigns to each index {\tt 1\leq i\leq n} the {\tt i}-th elementary symmetric
    function. If {\tt i} is outside the given bounds, an error is returned.
  
  Example
    R = symmetricRing(QQ,5,EHPVariables => (a,b,c));
    R.eVariable 3

SeeAlso
  hVariable
  pVariable
///

doc ///
Key
  hVariable
Headline
  Complete symmetric functions in a Symmetric ring
Description
  Text
    For a Symmetric ring {\tt R} of dimension {\tt n}, {\tt R.hVariable} is a function 
    which assigns to each index {\tt 1\leq i\leq n} the {\tt i}-th complete symmetric
    function. If {\tt i} is outside the given bounds, an error is returned.
  
  Example
    R = symmetricRing(QQ,2,EHPVariables => (x,y,z));
    R.hVariable 2

SeeAlso
  eVariable
  pVariable
///

doc ///
Key
  pVariable
Headline
  Power-sum symmetric functions in a Symmetric ring
Description
  Text
    For a Symmetric ring {\tt R} of dimension {\tt n}, {\tt R.pVariable} is a function 
    which assigns to each index {\tt 1\leq i\leq n} the {\tt i}-th power-sum symmetric
    function. If {\tt i} is outside the given bounds, an error is returned.
  
  Example
    R = symmetricRing(QQ,4);
    R.pVariable 2

SeeAlso
  eVariable
  hVariable
///

doc ///
   Key
     (numgens,SchurRing)
   Headline 
     Number of generators of Schur ring.
   Description
      Text
      
     	  Given a Schur ring {\tt S}, the function {\tt numgens} outputs the number
	  of generators of {\tt S}. This is equal to the relative dimension of {\tt S} 
	  over its base ring, and also to the maximal number of parts of a partition
	  allowed as an index for the elements of {\tt S}.
      
      Example
      	  R = schurRing(QQ,r,6);
	  numgens R
	  S = schurRing(s);
	  numgens S
///

doc ///
   Key
     (schurRing,Ring)
   Headline 
     The Schur ring corresponding to a given Symmetric ring.
   Usage
     S = schurRing R
   Inputs
     R:Ring
   Outputs
     S:SchurRing
   Description
      Text
      
     	  Given a ring {\tt R}, the function {\tt schurRing} attempts to return a
	  Schur ring {\tt S} that is associated to {\tt R} in a natural way. Namely, if
	  the attribute {\tt R.Schur} points to a Schur ring, then the function returns
	  that ring. If {\tt R} is already a Schur ring, then the ring {\tt R} is returned. 
	  Otherwise, if the Schur level of {\tt R} is at least one, then the function 
	  constructs a Schur ring over the base ring {\tt A} of {\tt R}, having the same
	  relative dimension over {\tt A} as {\tt R}. If the Schur level of {\tt R} is zero, then
	  an error is returned.
      
      Example
      	  R = schurRing(QQ,r,6);
	  schurRing R
	  Q = symmetricRing(QQ,3);
	  A = schurRing Q;	
	  schurRing Q
   SeeAlso
     symmetricRing
///

doc ///
   Key
     (symmetricRing,Ring)
   Headline 
     The Symmetric ring corresponding to a given (Schur) ring.
   Usage
     R = symmetricRing S
   Inputs
     S:Ring
   Outputs
     R:Ring
   Description
      Text
      
     	  Given a (Schur) ring {\tt S}, the function {\tt symmetricRing} returns a
	  (Symmetric) ring {\tt R} that is associated to {\tt S} in a natural way. Namely, if
	  the attribute {\tt S.symmetricRing} points to a ring, then the function returns
	  that ring. If {\tt S} is not a Schur ring, then the function returns {\tt S}.
	  Otherwise, if {\tt S} is a Schur ring, then the function 
	  constructs a polynomial ring over the Symmetric ring {\tt R_A} of the base ring {\tt A} of 
	  {\tt R}, having the same relative dimension over {\tt R_A} as {\tt S} over {\tt A}.
      
      Example
      	  A = schurRing(QQ,a,6);
	  B = schurRing(A,b,3);
	  symmetricRing B
	  symmetricRing ZZ
   SeeAlso
     schurRing
///

doc ///
   Key
     toS
     (toS,RingElement)
     (toS,RingElement,SchurRing)
   Headline
     Schur (s-) basis representation
   Usage
     fs = toS f
     fs = toS(f,S)
   Description
      Text

    	Given a symmetric function {\tt f}, the function 
        {\tt toS} yields a representation of {\tt f} as a linear
	combination of Schur functions. 

     	If {\tt f} is an element of a Symmetric ring {\tt R} and the output Schur ring {\tt S}
	is not specified, then the output {\tt fs} is an element of the Schur ring 
	associated to {\tt R} (see @TO schurRing@).
        
      Example
        R = symmetricRing(QQ,4);
        fs = toS(e_1*h_2+p_3)
        S = schurRing(s,2);
	toS(fs,S)
	
      Text
      
        This also works over tensor products of Symmetric/Schur rings.
	
      Example
        R = symmetricRing(4, EHPVariables => (a,b,c), SVariable => r);
	S = symmetricRing(R, 2, EHPVariables => (x,y,z), SVariable => s);
	T = symmetricRing(S, 3, SVariable => t);
	A = schurRing T;
	f = a_3*x_2*e_1 - b_1*z_2*p_3
	toS f
	
   SeeAlso
     toH
     toE
     toP
///

doc ///
  Key
    toE
    (toE,RingElement)
  Headline
     Elementary symmetric (e-) basis representation
  Usage
     fe = toE f
  Inputs
     f:RingElement
       element of a Symmetric or Schur ring
  Outputs
     fe:RingElement
        element of a Symmetric ring
  Description
      Text

          Given a symmetric function {\tt f}, the function 
          {\tt toE} yields a representation of {\tt f} as a polynomial
	  in the elementary symmetric functions.

  	  If {\tt f} is an element of a Schur ring {\tt S} then the output {\tt fe} is an 
	  element of the Symmetric ring associated to {\tt S} (see @TO symmetricRing@).

      Example
      	  R = symmetricRing 7;
	  toE(h_3*e_3)
	  S = schurRing(s,4)
	  toE S_{3,2,1}

      Text

        This also works over tensor products of Symmetric/Schur rings.
	
      Example
        R = schurRing(r, 4, EHPVariables => (a,b,c));
	S = schurRing(R, s, 2, EHPVariables => (x,y,z));
	T = schurRing(S, t, 3);
	A = symmetricRing T;
	f = (r_1+s_1+t_1)^2
	toE f
	
  SeeAlso
    toH
    toS
    toP
///

doc ///
  Key
    toH
    (toH,RingElement)
  Headline
     Complete symmetric (h-) basis representation
  Usage
     fh = toH f
  Inputs
     f:RingElement
       element of a Symmetric or Schur ring
  Outputs
     fh:RingElement
        element of a Symmetric ring
  Description
      Text

          Given a symmetric function {\tt f}, the function 
          {\tt toH} yields a representation of {\tt f} as a polynomial
	  in the complete symmetric functions.

  	  If {\tt f} is an element of a Schur ring {\tt S} then the output {\tt fh} is an 
	  element of the Symmetric ring associated to {\tt S} (see @TO symmetricRing@).

      Example
      	  R = symmetricRing 7;
	  toH(h_3*e_3)
	  S = schurRing(s,4)
	  toH S_{3,2,1}

      Text

        This also works over tensor products of Symmetric/Schur rings.
	
      Example
        R = schurRing(r, 4, EHPVariables => (a,b,c));
	S = schurRing(R, s, 2, EHPVariables => (x,y,z));
	T = schurRing(S, t, 3);
	A = symmetricRing T;
	f = (r_1+s_1+t_1)^2
	toH f
	
  SeeAlso
    toE
    toS
    toP
///

doc ///
  Key
    toP
    (toP,RingElement)
  Headline
     Power-sum (p-) basis representation
  Usage
     fp = toP f
  Inputs
     f:RingElement
       element of a Symmetric or Schur ring
  Outputs
     fp:RingElement
        element of a Symmetric ring
  Description
      Text

          Given a symmetric function {\tt f}, the function 
          {\tt toP} yields a representation of {\tt f} as a polynomial
	  in the power-sum symmetric functions.

  	  If {\tt f} is an element of a Schur ring {\tt S} then the output {\tt fp} is an 
	  element of the Symmetric ring associated to {\tt S} (see @TO symmetricRing@).

      Example
      	  R = symmetricRing 7;
	  toP(h_3*e_3)
	  S = schurRing(s,4)
	  toP S_{3,2,1}

      Text

        This also works over tensor products of Symmetric/Schur rings.
	
      Example
        R = schurRing(r, 4, EHPVariables => (a,b,c));
	S = schurRing(R, s, 2, EHPVariables => (x,y,z));
	T = schurRing(S, t, 3);
	A = symmetricRing T;
	f = (r_1+s_1+t_1)^2
	toP f
	
  SeeAlso
    toH
    toS
    toE
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
    in {\tt R}. The default option is to represent this symmetric
    function in terms of {\tt e-}polynomials.
  
  Example
    R = symmetricRing(QQ,10);
    jacobiTrudi({3,2,2,1},R)
    jacobiTrudi(new Partition from {4,4,1},R,EorH => "H")
    toS oo
///

doc///
   Key
     EorH
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
       larger than the length of {\tt lambda}, then it is
       computationally less expensive to set the option {\tt EorH}
       to {\tt "H"}. Otherwise, the default value {\tt "E"} is more
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
  Plethystic operations on representations
Usage
  pl = plethysm(f,g)
  pl = f @ g
Inputs
  f:RingElement
    element of Symmetric ring or Schur ring
  g:RingElement
    element of Symmetric ring or Schur ring
Outputs
  pl:RingElement
     element of the ring of {\tt g}
Description
  Text
    Given a symmetric functions {\tt f} and the character {\tt g} of a virtual representation of a product
    of general linear and symmetric groups, the method computes the character of the
    plethystic composition of {\tt f} and {\tt g}. The result of this operation will be an element of
    the ring of {\tt g}. We use the binary operator @TO symbol \@ @ as a synonym for the plethysm function.

  Example
    R = symmetricRing(QQ,5);
    pl = plethysm(h_2,h_3)
    toS pl
    S = schurRing(QQ,q,3);
    h_2 @ q_{2,1}
    plethysm(q_{2,1},q_{2,1})
    T = schurRing(S,t,2,GroupActing => "Sn");
    plethysm(q_{1,1,1}-q_{2,1}+q_{3},q_{2,1}*t_2-t_{1,1})
    p_3 @ (q_{2,1}*t_2-t_{1,1})
///

doc ///
Key
  (plethysm,BasicList,RingElement)
Headline
  Plethystic operations on representations
Usage
  pl = plethysm(lambda,g)
Inputs
  lambda:BasicList
         nonincreasing sequence of positive integers, or partition
  g:RingElement
    element of Symmetric ring or Schur ring
Outputs
  pl:RingElement
     element of the ring of {\tt g}
Description
  Text
    
    The method computes the character of the representation obtained by applying the Schur functor
    {\tt S_{\lambda}} to the representation with character {\tt g}, where
    {\tt \lambda} is a partition.

  Example
    R = symmetricRing(QQ,3);
    S = schurRing(QQ,q,3);
    toE plethysm({2,1},e_1*e_2-e_3)
    plethysm({2,1,1},q_{1,1})
    T = schurRing(S,t,4,GroupActing => "Sn");
    plethysm({1,1},q_1*t_{3,1})
///

doc ///
Key
  (plethysm,RingElement,ClassFunction)
  (plethysm,BasicList,ClassFunction)
Headline
  Plethystic operations on class functions
Usage
  pl = plethysm(f,cF)
  pl = plethysm(lambda,cF)
Description
  Text
    
    These methods describe the result of applying plethystic operations to a virtual
    character of a symmetric group. These operations are described either via a symmetric
    function {\tt f}, or a partition {\tt lambda}. Since {\tt cF} corresponds to an {\tt S_n}-
    representation, the option @TO GroupActing@ is irrelevant in this case.
    
  Example
    cF = new ClassFunction from {{2} => 1, {1,1} => -1};
    pl1 = plethysm({1,1},cF)
    R = symmetricRing 5;
    pl2 = plethysm(e_1+e_2,cF)
    S = schurRing R;
    symmetricFunction(cF,S)
    symmetricFunction(pl1,S)
    symmetricFunction(pl2,S)
///
-*
doc ///
Key
  (exteriorPower,ZZ,RingElement)
Headline
  Exterior power of a representation
Usage
  ep = exteriorPower(n,rep)
Inputs
  n:ZZ
  rep:RingElement
      an element of a SchurRing
Outputs
  ep:RingElement
Description
  Text
  
     Given a representation {\tt rep} of a product of general linear
     groups, and a positive integer {\tt n}, the function returns the
     {\tt n}-th exterior power of this representation.
     
  Example
     S = schurRing(QQ,s,2)
     T = schurRing(S,t,3)
     exteriorPower(4,s_{1}+t_{1})
///

doc ///
Key
  (symmetricPower,ZZ,RingElement)
Headline
  Symmetric power of a representation
Usage
  ep = symmetricPower(n,rep)
Inputs
  n:ZZ
  rep:RingElement
      an element of a SchurRing
Outputs
  ep:RingElement
Description
  Text
  
     Given a representation {\tt rep} of a product of general linear
     groups, and a positive integer {\tt n}, the function returns the
     {\tt n}-th symmetric power of this representation.
     
  Example
     S = schurRing(QQ,s,2)
     T = schurRing(S,t,3)
     symmetricPower(4,s_{1}+t_{1})
///
*-

doc ///
Key
  schurResolution
  (schurResolution,RingElement,List,List)
  (schurResolution,RingElement,List)
Headline
  Compute an ``approximate'' equivariant resolution of a module.
Usage
  resol = schurResolution(rep,M,lS)
  resol = schurResolution(rep,M)
Inputs
  rep:RingElement
      element of a SchurRing
  M:List
    list of representations, corresponding to the homogeneous components of a module {\tt M}.
  lS:List
    list of representations, corresponding to the homogeneous components of a polynomial ring {\tt S}.
Outputs
  resol:List
Description
  Text
  
     Given a representation {\tt rep} of a (product of) general linear
     or symmetric group(s) {\tt G}, we consider the symmetric algebra {\tt S = Sym(rep)}
     and an {\tt S}-module {\tt M} which is also a {\tt G}-module in such
     a way that the {\tt S}-module structure on {\tt M} respects the 
     {\tt G}-action. More generally, {\tt S} can be any graded ring, of which one inputs only
     finitely many homogeneous components as a list {\tt lS} of characters of {\tt G}. The main reason
     why we allow this generality is because most of the time it is computationally expensive to calculate
     the symmetric powers of the representation {\tt rep}, so we give the user the option to compute these
     symmetric powers by different methods and use the results as input for the schurResolution routine.
     
     We are interested in computing an equivariant 
     resolution of {\tt M}. This depends on both the {\tt G}- and {\tt S}-module structure 
     of {\tt M} in general, but in many examples that occur in practice, it turns out that
     the differentials in the resolution have maximal rank among all 
     {\tt G}-module homomorphisms between the free modules in the resolution.
     We will therefore assume that this is the case for the module {\tt M} that we are
     trying to resolve, and thus disregard its {\tt S}-module structure.
     
     More precisely, the assumptions that we make about {\tt M} are as follows: {\tt M} is 
     a graded {\tt S}-module, with {\tt M_i = 0} for {\tt i<0}, where the grading on {\tt S} is standard,
     given by setting the degrees of the elements of {\tt rep} equal to 1. Since we assumed
     that the {\tt G}-structure of {\tt M} determines the syzygies, all the relevant
     information is concentrated in finitely many homogeneous components of {\tt M} (namely
     up to {\tt reg(M)+pd(M)}, the sum of the regularity and the projective dimension of
     {\tt M}). We will thus assume that {\tt M}
     is given as a list of {\tt G}-representations, corresponding to (a subset of) the
     relevant homogeneous components. The function {\tt schurResolution} takes as 
     inputs the representation {\tt rep}, the module {\tt M}, and as optional arguments a {\tt DegreeLimit}
     {\tt d}, and a {\tt SyzygyLimit} {\tt c}. The ring {\tt S} itself can occur as input data, being
     described as a list of {\tt G}-representations, just like {\tt M}.
     The routine outputs the generators of degree at most {\tt d} of the 
     first {\tt c+1} syzygy modules (from {\tt 0} to {\tt c}). They are listed as a 
     sequence of pairs, consisting of the degree of the generators of the syzygy modules 
     together with the characters of the {\tt G}-representations they correspond to. 
     If the syzygy bound {\tt c} is not given,
     then all syzygy modules are computed. If the degree bound {\tt d} is not given, then
     it is assumed to be equal to the largest degree among the homogeneous components of
     {\tt M} in the input, i.e. one less than the length of the @TO List@ {\tt M}.
     
     The example below computes the resolution of the quadratic Veronese 
     surface in {\tt P^5}.
      
  Example
    S = schurRing(QQ,s,3)
    rep = s_{2}
    M = {1_S,s_{2},s_{4},s_{6},s_{8},s_{10},s_{12}}
    schurResolution(rep,M)

  Text
  
    Next, we compute the syzygies of degree at most {\tt 7} in the resolution 
    of the cubic Veronese embedding of {\tt P^2}.
    
  Example
    rep = s_{3}
    M = {1_S,s_{3},s_{6},s_{9},s_{12},s_{15},s_{18},s_{21},s_{24},s_{27}}
    d = 7
    schurResolution(rep,M,DegreeLimit => d)

  Text
    
    We can compute the resolution of the ideal of {\tt 2\times 2} minors of a {\tt 3\times 4}
    matrix, which corresponds to the Segre embedding of {\tt P^2\times P^3}:
    
  Example
    T = schurRing(S,t,4)
    rep = s_1 * t_1
    M = {1_T} | apply(splice{1..8},i -> s_i * t_i)
    schurResolution(rep,M)

  Text
  
    The following example computes the equivariant resolution of the residue field of a 
    polynomial ring in {\tt n=5} variables, with respect to the action of the symmetric 
    group {\tt S_n}.
  
  Example
    n = 5;
    S = schurRing(QQ,s,n,GroupActing => "Sn");
    rep = s_n + s_{n-1,1};
    M = {s_n}
    schurResolution(rep,M,DegreeLimit => n)

  Text
  
    Generalizing this, we can compute the equivariant resolution of the quotient of the
    polynomial ring in {\tt n=5} variables by the ideal of square-free monomials of
    degree two, with respect to the action of the symmetric group {\tt S_n}.
  
  Example
    M = {s_n} | splice{n:rep};
    schurResolution(rep,M)    

///

doc ///
Key
  [schurResolution,DegreeLimit]
Headline
  Specifies the maximal degree of syzygies to be computed
Description
  Text
    This is an optional argument for the @TO schurResolution@ routine. It specifies an upper bound for the
    degrees of the generators of the syzygy modules in the equivariant resolution of an equivariant module {\tt M}
    to be computed by the routine. If a {\tt DegreeLimit} is not specified, then it is assumed to be equal to the 
    maximal degree in which the module {\tt M} is specified as a representation.
    
  Example
    A = schurRing(a,3,GroupActing => "Sn");
    B = schurRing(A,b,2);
    rep = (a_3 + a_{2,1}) * b_1
    d = dim rep
    M = {a_3 * 1_B};
    sR = schurResolution(rep,M,DegreeLimit => d)

SeeAlso
  [schurResolution,SyzygyLimit]
///
    
doc ///
Key
  [schurResolution,SyzygyLimit]
Headline
  Specifies the number of syzygy modules to be computed
Description
  Text
    This is an optional argument for the @TO schurResolution@ routine. It specifies an upper bound for the
    number of syzygy modules in the equivariant resolution of an equivariant module {\tt M} to be computed 
    by the routine. If a {\tt SyzygyLimit} is not specified, then all syzygy modules are computed.
    
    The example below computes the {\tt 0}-th to {\tt 3}-rd syzygy modules of the {\tt 5}-th Veronese embedding
    of {\tt P^2}.
    
  Example
    S = schurRing(s,3);
    rep = s_{5};
    M = {1_S,s_{5},s_{10},s_{15},s_{20},s_{25},s_{30}};
    schurResolution(rep,M,SyzygyLimit => 3)

SeeAlso
  [schurResolution,DegreeLimit]
///
    
doc ///
  Key
    schurLevel
    (schurLevel,Ring)
  Headline
    Number of SchurRings the ring is a tensor product of.
  Usage
    lev = schurLevel(R)
  Inputs
    R:Ring
  Outputs
    lev:ZZ
  Description
    Text
    
      For the representation ring {\tt R} of a product of {\tt lev}
      general linear groups, the function returns {\tt lev}. If {\tt R}
      is not a representation ring, then the function returns 0.
      
    Example
      R = schurRing(QQ,r,3);
      S = schurRing(R,s,5);
      T = schurRing(S,t,2);
      schurLevel R
      schurLevel S
      schurLevel T
      schurLevel QQ  
///

doc ///
  Key
    (partitions,Set,BasicList)
  Headline
    Partitions of a set
  Usage
    par = partitions(S,L)
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
      partitions(set{1,2,3,4},{2,1,1})
      partitions(set{a,b,c,d,e},new Partition from {3,2})
///  

-*
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
   symmetricFunction
   classFunction
///
*-

doc ///
Key
  SchurRingElement
Headline
  A type describing elements of a SchurRing
Description
  Example
    S = schurRing(s,5)
    a = s_{3,2,1}
    instance(a,SchurRingElement)

    T = schurRing(S,t,3,GroupActing => "Sn")
    b = t_{2,1}+t_3
    instance(a*b,SchurRingElement)
///

doc ///
   Key
     (dim,List,SchurRingElement)
     (dim,Thing,SchurRingElement)
     (dim,SchurRingElement)
   Headline
     dimension of representation
   Usage
     d = dim(lis,s)
     d = dim(n,s)
     d = dim s
   Inputs
     lis: List
     	  or @TO Thing@
     s: SchurRingElement
   Outputs
     d: ZZ
        or @TO Expression@
   Description
     Text
     
       The method returns the dimension of the virtual representation whose character is represented by {\tt s}.
       
     Example
       S = schurRing(s,3)
       dim s_2
       T = schurRing(t,4,GroupActing => "Sn")
       dim t_{2,2}
       U = schurRing(T,u,3)
       dim (t_{2,2}*u_2)

     Text
     
       If {\tt S} is a @TO SchurRing@ of level 1, the ring of polynomial representations of some {\tt GL(V)}, it
       may sometimes be convenient to compute dimensions of {\tt GL(V)-}representations symbolically, without 
       specifying the dimension of {\tt V}. Letting {\tt n} denote the parameter corresponding to {\tt dim(V)} we
       have for example
       
     Example
       S = schurRing(s,3)
       dim(n,s_2)
       dim(n,s_{1,1})
       dim(n,s_{2,1})
     
     Text       
     
       Similar calculations make sense over products of general linear groups. The dimensions of the representations 
       can be computed symbolically as functions of a number of parameters
       equal to the @TO schurLevel@ of the ring. The parameters corresponding to levels where the group acting 
       is a symmetric group don't have a good interpretation, so they are disregarded in the dimension calculation.
       The order of the input parameters is the descending order of the @TO schurLevel@s: in the example below
       {\tt a} corresponds to {\tt Q}, {\tt b} corresponds to {\tt T} and {\tt c} corresponds to {\tt S}.
       
     Example
       S = schurRing(s,3)
       T = schurRing(S,t,4)
       Q = schurRing(T,q,5,GroupActing => "Sn")
       dExpr = dim({a,b,c},s_{2}*t_{1,1}*q_{4,1})
       P = QQ[a,b,c]
       value dExpr
       dim({1,2,3},s_{2}*t_{1,1}*q_{4,1})
///


doc ///
Key
  ClassFunction
  (symbol +,ClassFunction,ClassFunction)
  (symbol -,ClassFunction,ClassFunction)
  (symbol *,ClassFunction,ClassFunction)
  (symbol *,ClassFunction,RingElement)
  (symbol *,RingElement,ClassFunction)
  (symbol *,ClassFunction,Number)
  (symbol *,Number,ClassFunction)
  (symbol ==,ClassFunction,ClassFunction)
Headline
  The class of all Class functions
Description
  Text
    A class function (or virtual character of a symmetric group {\tt S_n}) is a function that is constant
    on the conjugacy classes of {\tt S_n}. Class functions for {\tt S_n} are in one to one
    correspondence with symmetric functions of degree {\tt n}. The class functions corresponding
    to actual representations of {\tt S_n} are called {\tt characters}.
  
    The character of the standard representation of {\tt S_3} is

  Example
    S = schurRing(QQ,s,3);
    classFunction(s_{2,1})
  
  Text
    The character of the sign representation of {\tt S_5} is

  Example
    S = schurRing(QQ,s,5);
    classFunction(s_{1,1,1,1,1})
  
  Text
    We can go back and forth between class functions and symmetric functions.
  
  Example
    R = symmetricRing(QQ,3);
    cF = new ClassFunction from {{1,1,1} => 2, {3} => -1};
    sF = symmetricFunction(cF,R)
    toS sF
    classFunction sF
  
  Text
    We can add, subtract, multiply, scale class functions:
    
  Example
    S = schurRing(QQ,s,4);
    c1 = classFunction(S_{2,1,1}-S_{4});
    c2 = classFunction(S_{3,1});
    c1 + c2
    c1 * c2
    3*c1 - c2*2
    
///

doc ///
Key
  (degree,ClassFunction)
Headline
  Degree of virtual character
Description
  Text
    For a virtual character {\tt ch} of a symmetric group on {\tt n} letters, the degree
    of {\tt ch} is {\tt n}.
    
  Example
    S = schurRing(s,5);
    ch = classFunction s_(3,1,1)
    degree ch
///

doc ///
Key
  symmetricFunction
  (symmetricFunction,ClassFunction,Ring)
Headline
  Converts class function to symmetric function
Usage
  f = symmetricFunction(ch,S)
Inputs
  ch:ClassFunction
  S:Ring
    a Symmetric or Schur ring
Outputs
  f:RingElement
    element of a Symmetric or Schur ring
Description
  Text
    Given a virtual character {\tt cF} of a
    symmetric group, and given a Symmetric ring {\tt S}, 
    the method computes the corresponding symmetric function
    as an element of {\tt S}.
    
  Example
    S = symmetricRing(QQ,4);
    cF = new ClassFunction from {{1,1,1,1}=>24};
    symmetricFunction(cF,S)
    symmetricFunction(cF,schurRing S)
SeeAlso
  classFunction
--  chi
///

doc ///
Key
  classFunction
  (classFunction,RingElement)
Headline
  Converts symmetric function to class function
Usage
  ch = classFunction(f)
Inputs
  f:RingElement
    element of a Symmetric ring
Outputs
  ch:ClassFunction
Description
  Text
    Given a symmetric function {\tt f}, homogeneous of degree {\tt N}, 
    the method computes the corresponding virtual character
    of the symmetric group {\tt S_N}.
    
    The character of the standard representation of {\tt S_5} is
    
  Example
    R = symmetricRing(QQ,5);
    classFunction(jacobiTrudi({4,1},R))
  Text

    The character of the second exterior power of the standard representation of {\tt S_5} is
    
  Example
    R = symmetricRing(QQ,5);
    classFunction(jacobiTrudi({3,1,1},R))

SeeAlso
  symmetricFunction
--  chi
///

doc ///
Key
  (classFunction,BasicList)
Headline
  Character of irreducible representation of symmetric group
Usage
  ch = classFunction(l)
Inputs
  l:BasicList
    partition
Outputs
  ch:ClassFunction
Description
  Text
    Given a partition {\tt l} of {\tt N}, the method computes the character of the irreducible
    {\tt S_N}-representation corresponding to the partition {\tt l}.
    
  Example
    R = symmetricRing(QQ,7);
    cF = classFunction({3,2,1})
    toS(symmetricFunction(cF,R))
SeeAlso
  symmetricFunction

///

doc ///
Key
  scalarProduct
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
    R = symmetricRing(QQ,10);
    S = schurRing(QQ,s,10);
    scalarProduct(h_1^10,s_{4,3,2,1})
SeeAlso
  internalProduct
///

doc ///
Key
  (scalarProduct,RingElement,RingElement)
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
    R = symmetricRing(QQ,5);
    S = schurRing R
    scalarProduct(h_5,p_5)
    scalarProduct(S_{4,1},p_5)
  
  Text
  
    Indeed, the coefficients of {\tt s_5} and {\tt s_{4,1}} in the
    s-basis expansion of {\tt h_5} are as computed above:
    
  Example
    R = symmetricRing(QQ,5);
    toS p_5
///

doc ///
Key
  (scalarProduct,ClassFunction,ClassFunction)
Headline
  Standard scalar product of class functions
Usage
  sp = scalarProduct(ch1,ch2)
Inputs
  ch1:ClassFunction
  ch2:ClassFunction
Outputs
  sp:QQ
Description
  Text
    
    Given virtual characters {\tt ch1} and {\tt ch2}, the method
    computes the standard pairing between {\tt ch1} and {\tt ch2}.
    
  Example
    ch1 = new ClassFunction from {{3,2} => 2, {2,2,1} => -2, {3,1,1} => 2, {5} => 1};
    ch2 = new ClassFunction from {{2,2,1} => -2, {5} => 1, {1,1,1,1,1} => 5, {3,2} => 3, {4,1} => -2};
    scalarProduct(ch1,ch2)
///

doc ///
Key
  internalProduct
Headline
  Internal product of symmetric functions/class functions
Description
  Text
  
    This method computes the internal (Kronecker) product of two homogeneous symmetric
    functions of the same degree. If we think of these functions as being
    virtual characters of some symmetric group, then their internal product
    is just the character of the tensor product of the corresponding virtual
    representations. We use the binary operator @TO symbol **@ as a shorthand for
    @TO internalProduct@.

    The complete symmetric function of degree {\tt n} corresponds
    to the trivial {\tt S_n}-representation and is therefore
    the unit of the representation ring of {\tt S_n}:
  
  Example
    R = symmetricRing(QQ,5);
    S = schurRing(QQ,s,3);
    internalProduct(h_3,s_{2,1})
    toE(h_3 ** e_3)
  Text
  
    The square of the sign representation is the trivial representation:
    
  Example
    R = symmetricRing(QQ,5);
    toH internalProduct(e_3,e_3)
SeeAlso
  scalarProduct
///

doc ///
Key
  (internalProduct,RingElement,RingElement)
Headline
  Kronecker product of symmetric functions
Usage
  ip = internalProduct(f1,f2)
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
     R = symmetricRing(QQ,6);
     S = schurRing(QQ,s,6);
     toE(h_3**e_3)
     Q = schurRing(QQ,q,6);
     internalProduct(s_{3,3},q_{4,2})   
  Text
   
    An error is returned if {\tt f1} and {\tt f2} don't have the
    same degree.
///

doc ///
Key
  (internalProduct,ClassFunction,ClassFunction)
Headline
  Tensor product of virtual representations
Usage
  ip = internalProduct(ch1,ch2)
Inputs
  ch1:ClassFunction
  ch2:ClassFunction
Outputs
  ip:ClassFunction
Description
  Text
    
    Given virtual characters {\tt ch1} and {\tt ch2}, the method
    computes the character of the tensor product of corresponding
    virtual representations of the symmetric group.
    
  Example
    ch1 = new ClassFunction from {{4,4} => 2, {8} => -1, {5,2,1} => 2, {3,2,2,1} => 1};
    ch2 = new ClassFunction from {{2,2,2,2} => -4, {5,2,1} => 1, {3,2,2,1} => 3};
    internalProduct(ch1,ch2)
    ch1 * ch2
///

doc ///
Key
  centralizerSize
  (centralizerSize,List)
Headline 
  Size of the centralizer of a permutation
Usage 
  n = centralizerSize(rho)
Inputs 
  rho:List
Outputs 
  n:ZZ
Description
  Text

    {\tt rho} is a list representing the cycle type of some permutation: the i-th entry in {\tt rho} is the number of cycles of length i of the permutation.
    The output of the function {\tt centralizerSize} is the size of the centralizer in the symmetric group of any permutation of cycle type {\tt rho}. The cycle type {\tt rho}
    corresponds to a partition {\tt lambda}, in which case {\tt centralizerSize(rho)} is also the value of the square norm of the symmetric function {\tt p_{lambda}}.

  Example
    centralizerSize{1,1,1}
    R = symmetricRing(QQ,6);
    u = p_1 * p_2 * p_3;
    scalarProduct(u,u)
///

doc ///
  Key
    Memoize
  Headline
    Option to record values of the jacobiTrudi function
  Description
    Text
    
      This is an optional argument for the @TO jacobiTrudi@
      function, allowing one to store its values
      in order to speed up computations.
///

doc ///
Key
  SVariable
  [schurRing,SVariable]
  [symmetricRing,SVariable]
Headline
  Specifies symbol representing s-functions
Description
  Text
    This is an optional argument for the constructor of a Symmetric ring. It indicates the
    symbol to be used to denote s-functions in the associated Schur ring. The default value
    is {\tt s}.
   
  Example
    R = symmetricRing(QQ,5,SVariable => getSymbol"s");
    S = schurRing R
    s_2^2

SeeAlso
  EHPVariables
///

doc ///
Key
  EHPVariables
  [schurRing,EHPVariables]
  [symmetricRing,EHPVariables]
Headline
  Specifies sequence of symbols representing e-, h-, and p-functions
Description
  Text
    This is an optional argument for the constructor of a Symmetric or Schur ring. It indicates the
    symbols to be used to denote e-, h-, and p-functions in the associated Symmetric ring. The
    default values are {\tt (e,h,p)}.
   
  Example
    S = schurRing(QQ,s,4,EHPVariables => (getSymbol"a",getSymbol"b",getSymbol"c"));
    R = symmetricRing S
    epol = toE s_{2,2,2}
    toS epol

SeeAlso
  SVariable
///

doc ///
Key
  GroupActing
  [schurRing,GroupActing]
  [symmetricRing,GroupActing]
Headline
  Specifies the group that is acting
Description
  Text
    This is an optional argument for the @TO schurRing@ and @TO symmetricRing@ functions. 
    When the exterior or symmetric powers of a symmetric function {\tt g} are computed, the result
    depends on whether {\tt g} is interpreted as a virtual representation of a general 
    linear or symmetric group. The option {\tt GroupActing} specifies the interpretation 
    to be considered. Its possible values are {\tt "GL"} and {\tt "Sn"}, with the former 
    being the default.
    
  Example
    S = schurRing(s,2);
    exteriorPower(3,s_2)
    T = schurRing(t,2,GroupActing => "Sn");
    symmetricPower(2,t_{1,1})
      
  Text
  
    The first example computes the decomposition of {\tt \Lambda^3(Sym^2(V))} into irreducible
    {\tt GL(V)}-representations, while the second one computes the
    second symmetric power of the sign representation of the symmetric group {\tt S_2}.
///


--------------------
-- test Jacobi-Trudi
--------------------
TEST ///
E = symmetricRing(QQ,5)
f = jacobiTrudi({4,1},E)
g = toS f
G = ring g
assert (g == G_{4,1})
///

TEST ///
E = symmetricRing(QQ,5)
f = jacobiTrudi({2,1},E)
g = toS f
G = ring g
assert (g == G_{2,1})
///

TEST ///
E = symmetricRing(QQ,13)
f = jacobiTrudi({7,4,2},E)
g = toS f
G = ring g
assert (toS f == G_{7,4,2})
///

TEST ///
P = symmetricRing(QQ,6)
f = toS plethysm(jacobiTrudi({2},P), jacobiTrudi({2},P))
G = ring f
assert(f == G_{4}+G_{2,2})
///

TEST ///
Q = symmetricRing(QQ,5)
--S = schurRing(QQ,q,4)
S = schurRing Q
f = toS(plethysm(jacobiTrudi({3},Q), jacobiTrudi({2},Q)))
--assert(dim f == 220)
assert(dim(4,f) == 220)
///
------------------------
-- end test Jacobi-Trudi
------------------------

-----------
-- test dim
-----------

TEST ///
R = schurRing(r,3,GroupActing => "Sn")
S = schurRing(R,s,2)
T = schurRing(S,t,4,GroupActing => "Sn")
assert( (dim(r_1)) == 1 )
assert( (dim(s_2)) == 3 )
assert( (dim(s_3)) == 4 )
assert( (dim(t_4)) == 1 )
assert( (dim(r_1 * s_2 * t_3)) == 3 )
assert( (dim(r_1 * s_3 + t_4)) == 5 )
///

---------------
-- end test dim
---------------

---------------------
-- test plethysm, toS
---------------------
TEST ///
R = symmetricRing(QQ,4)
pl = plethysm({1,1},jacobiTrudi({2},R))
G = schurRing ring pl
assert(toS pl == G_{3,1})
///

TEST ///
R = symmetricRing(QQ,12)
pl = plethysm({1,1,1},jacobiTrudi({4},R))
assert(#listForm(toS pl) == 7)
///

TEST ///
R = symmetricRing(QQ, 9)
S = schurRing(QQ,q,3)
pl = plethysm(h_3,q_{2,1})
assert (dim(pl) == 120)
///

TEST ///
R = symmetricRing(QQ,3)
S = schurRing R
assert(toS(h_3 @ e_3) == S_{3,3,3})
///

TEST ///
S = schurRing(QQ,q,4)
assert(plethysm(q_{2,1},q_{1,1,1}) == q_{3,3,2,1})
///

TEST ///
R = symmetricRing(QQ, 12)
f = e_4
lambda = new Partition from {3}
assert(plethysm(lambda,f) == h_3 @ e_4)
///

TEST ///
schurRing(QQ,s,2)
assert(dim(plethysm(s_{2,1}+s_{3},s_{3})) == 40)
///

TEST ///
R = symmetricRing(QQ,20)
assert(#listForm(toS plethysm(h_5,h_4)) == 95)
///


TEST ///
R = symmetricRing(QQ, 10)
S = schurRing R
sch = toS(plethysm({2,1},h_3))
assert(dim(5,sch) == 14280)
///

TEST ///
R = symmetricRing 5
S = schurRing(s,3)
assert( ((h_2 + p_2) @ s_{2,1}) == 2*s_(4,2)-s_(4,1,1)-s_(3,3)+s_(3,2,1)+2*s_(2,2,2) )
///

TEST ///
S = schurRing(s,3)
T = schurRing(S,t,4,GroupActing => "Sn")
Q = schurRing(T,q,2)
assert( (s_2 @ q_{2,1}) == q_(4,2) )
assert( (s_{2,1} - s_{1,1,1} @ (t_3 + t_(2,1))) == -s_()*t_(1,1,1)+s_(2,1)*t_() )
assert( (plethysm({3},s_1 * t_1 * q_1)) == s_3*t_1*q_3+s_(2,1)*t_1*q_(2,1) )
assert( (plethysm({2,1},s_1 + t_1 + q_1)) == q_(2,1)+(t_1+s_1*t_())*q_2+(t_1+s_1*t_())*q_(1,1)+((2*s_1+s_())*t_1+(s_2+s_(1,1))*t_())*q_1+((s_2+s_(1,1)+s_1)*t_1+s_(2,1)*t_())*q_() )
assert( toH plethysm({2,1},s_1 + t_1 + q_1) == toH plethysm({2,1},toE(s_1 + t_1 + q_1)) )
///

----------------------------
-- end test of plethysm, toS
----------------------------

------------------------------------
----- symmetricPower & exteriorPower
------------------------------------

TEST ///
S = schurRing(s,5)
T = schurRing(S,t,3,GroupActing => "Sn")
assert( (symmetricPower(2,s_3)) == s_6+s_(4,2) )
assert( (symmetricPower(2,t_2)) == t_2 )
assert( (symmetricPower(2,s_3+t_2)) == (s_3+s_())*t_2+(s_6+s_(4,2))*t_() )
assert( (exteriorPower(3,s_3 * t_{2,1} - t_3)) == (s_(7,1,1)+s_(6,3)+s_(5,3,1)-s_(5,1)+s_(3,3,3)-s_(3,3)-s_())*t_3+(s_(8,1)+s_(7,2)+s_(7,1,1)+2*s_(6,3)+s_(6,2,1)+s_(5,4)+2*s_(5,3,1)-s_(5,1)+s_(4,3,2)+s_(3,3,3)-s_(3,3)+s_3)*t_(2,1)+(s_(7,1,1)+s_(6,3)-s_6+s_(5,3,1)-s_(4,2)+s_(3,3,3))*t_(1,1,1) )
assert( (exteriorPower(5,s_1 * t_1)) == s_(1,1,1,1,1)*t_1 )
///

TEST ///
R = symmetricRing(3,GroupActing => "Sn")
S = symmetricRing(R,4)
T = symmetricRing(S,2,GroupActing => "Sn")

a = R.hVariable 3
b = S.eVariable 4
c = T.pVariable 2

assert (toS(symmetricPower(3,a*b*c)) == symmetricPower(3,toS(a*b*c)))
assert (toS(exteriorPower(3,a*b*c)) == exteriorPower(3,toS(a*b*c)))
assert (toS(symmetricPower(3,a+b-c)) == symmetricPower(3,toS(a+b-c)))
assert (toS(exteriorPower(3,a*b-c)) == exteriorPower(3,toS(a*b-c)))
///

----------------------------------------
----- end symmetricPower & exteriorPower
----------------------------------------

-------------------------------------------------------------------
----- test characters of symmetric groups, scalarProd, internalProd
-------------------------------------------------------------------
TEST ///
R = symmetricRing(QQ,20)
S = schurRing(QQ,o,20)
assert(scalarProduct(o_{6,4,3,2,1},jacobiTrudi({3,3,3},symmetricRing S)*toP(o_{4,2,1})) == 2)
assert(scalarProduct(jacobiTrudi({6,4,3,2,1},R),jacobiTrudi({4,3,3,3,2,1},R)) == 0)
assert(scalarProduct(jacobiTrudi({6,4,3,2,1},R),o_{4,3,3,3,2,1}) == 0)
///

TEST ///
R = symmetricRing(QQ,5)
A = schurRing(QQ,a,4)
assert(internalProduct(e_2+h_2,a_{2}) == a_{2}+a_{1,1})
assert(toE internalProduct(a_{2},e_2+h_2) == toE p_1^2)
assert(dim internalProduct(a_{2,1}*a_{1},a_{2,2}) == 176)
///


TEST ///
R = symmetricRing(QQ,10)
ch1 = new ClassFunction from {{4,4} => 2, {8} => -1, {5,2,1} => 2, {3,2,2,1} => 1};
ch2 = new ClassFunction from {{2,2,2,2} => -4, {5,2,1} => 1, {3,2,2,1} => 3};
assert(toP symmetricFunction(internalProduct(ch1,ch2),R) == 1/8*p_1*p_2^2*p_3+1/5*p_1*p_2*p_5)
assert(toP symmetricFunction(ch1*ch2,R) == 1/8*p_1*p_2^2*p_3+1/5*p_1*p_2*p_5)
///

TEST ///
R = symmetricRing(QQ,4)
f = p_2^2
g = (e_2+h_2)^2
ch1 = classFunction(f)
ch2 = classFunction(g)
assert(symmetricFunction(internalProduct(ch1,ch2),R) == 0)
assert(internalProduct(f,g) == 0)
///
---------------------------------------------------------------------
--- end test characters of symmetric groups, scalarProd, internalProd
---------------------------------------------------------------------

---------------------------
--- test toS, toP, toE, toH
---------------------------
TEST ///
R = symmetricRing(QQ,6)
assert(toE(toS(e_1*e_2*e_3)) == e_1*e_2*e_3)
///

TEST ///
R = symmetricRing(QQ,5)
S = schurRing(QQ,q,3)
assert(toE(q_{2}) + e_2 == e_1^2)
///

TEST///
R = symmetricRing(QQ, 4)
assert(toP toE toH toE toH toP toE toE toP toH (p_1+p_2+p_3) == p_1+p_2+p_3)
///

TEST ///
R = symmetricRing(QQ,6)
S = schurRing R
toSf = map(S, R, apply(gens R, x -> toS(x)))
assert(toSf(e_1*e_2*e_3) == S_(3,2,1)+S_(3,1,1,1)+S_(2,2,2)+2*S_(2,2,1,1)+2*S_(2,1,1,1,1)+S_(1,1,1,1,1,1))
assert(toSf(h_1*h_2*h_3) == S_{1}*S_{2}*S_{3})
///

TEST ///
R = symmetricRing(QQ,7)
assert(toH toP toE (toS (jacobiTrudi({2,1},R))^2) == (h_1*h_2-h_3)^2)
///

TEST ///
S = schurRing(s,5,GroupActing => "Sn")
R = symmetricRing S
T = schurRing(S,t,3,EHPVariables => (getSymbol "eT", getSymbol "hT", getSymbol "pT"))
Q = symmetricRing T
a = toS((R.pVariable 2) * (R.eVariable 3))
b = a * (t_3 - t_{2,1})
c = toH(a + b)
d = toE(a - b)
f = toP b
assert( (a) == s_(3,1,1)-s_(2,2,1)-s_(1,1,1,1,1) )
assert( (b) == (s_(3,1,1)-s_(2,2,1)-s_(1,1,1,1,1))*t_3+(-s_(3,1,1)+s_(2,2,1)+s_(1,1,1,1,1))*t_(2,1) )
assert( (c) == (h_1^5-4*h_1^3*h_2+4*h_1*h_2^2+h_1^2*h_3-2*h_2*h_3)*hT_1*hT_2+(-2*h_1^5+8*h_1^3*h_2-8*h_1*h_2^2-2*h_1^2*h_3+4*h_2*h_3)*hT_3-h_1^5+4*h_1^3*h_2-4*h_1*h_2^2-h_1^2*h_3+2*h_2*h_3 )
assert( (d) == (-e_1^2*e_3+2*e_2*e_3)*eT_1^3+(3*e_1^2*e_3-6*e_2*e_3)*eT_1*eT_2+(-2*e_1^2*e_3+4*e_2*e_3)*eT_3+e_1^2*e_3-2*e_2*e_3 )
assert( (f) == (-(1/36)*p_1^3*p_2+(1/12)*p_1*p_2^2-(1/18)*p_2*p_3)*pT_1^3+((1/12)*p_1^3*p_2-(1/4)*p_1*p_2^2+(1/6)*p_2*p_3)*pT_1*pT_2+((1/9)*p_1^3*p_2-(1/3)*p_1*p_2^2+(2/9)*p_2*p_3)*pT_3 )
assert( (toH(c - d - 2*f)) == 0 )
assert( (toE(c - d - 2*f)) == 0 )
assert( (toP(c - d - 2*f)) == 0 )
///
-------------------------------
--- end test toS, toP, toE, toH
-------------------------------

-------------------------------------------------
--- test schurLevel, symmetricRing, schurRing
-------------------------------------------------

TEST ///
R = symmetricRing 5
S = schurRing R
S1 = schurRing(R,s1,3,GroupActing => "Sn")
R1 = symmetricRing S1
R2 = symmetricRing(R1,3,GroupActing => "Sn")
S2 = schurRing R2
assert( (schurLevel QQ) == 0 )
assert( schurLevel (ZZ/5) == 0 )
assert( (schurLevel R) == 1 )
assert( (schurLevel R1) == 2 )
assert( (schurLevel R2) == 3 )
assert( (schurLevel S) == 1 )
assert( (schurLevel S1) == 2 )
assert( (schurLevel S2) == 3 )
///

-----------------------------------------------------
--- end test schurLevel, symmetricRing, schurRing
-----------------------------------------------------

-----------------------
-- test centralizerSize
-----------------------

TEST ///
assert( (centralizerSize{3,2,1}) === 144 )
assert( (centralizerSize{1,1,1}) === 6 )
assert( (centralizerSize{3}) === 6 )
assert( (centralizerSize{5,2,1,1,1}) === 57600 )
assert( (centralizerSize{5,5,5}) === 13436928000 )
assert( (centralizerSize{4,4,2,2}) === 5308416 )
assert( (centralizerSize{1}) === 1 )
///

---------------------------
-- end test centralizerSize
---------------------------

-----------------------
-- test schurResolution
-----------------------

TEST ///
S = schurRing(QQ,s,3)
rep = s_{2}
M = {1_S,s_{2},s_{4},s_{6},s_{8},s_{10},s_{12}}
sR = schurResolution(rep,M)
assert( (#sR) == 4 )
assert( (sR#2#0#1) == s_(3,2,1) )
assert( (last last sR) == (4,s_(3,3,2)) )

rep = s_{3}
M = {1_S,s_{3},s_{6},s_{9},s_{12},s_{15},s_{18},s_{21},s_{24},s_{27}}
d = 7
sR = schurResolution(rep,M,DegreeLimit => d)
assert( (#sR) == 7 )
assert( (sR#2#0#0) == 3 )

l = apply(sR,i -> i / (j -> dim last j))
assert( (l) == {{1}, {27}, {105}, {189}, {189}, {105}, {27}} )

T = schurRing(S,t,4)
rep = s_1 * t_1
M = {1_T} | apply(splice{1..8},i -> s_i * t_i)
sR = schurResolution(rep,M)
assert( (last last sR) == (8,s_(3,3,2)*t_(2,2,2,2)) )
assert( (first first sR) == (0,t_()) )
assert( (sR#1#0) == (2,s_(1,1)*t_(1,1)) )
assert( (sR#4#1) == (6,s_(2,2,2)*t_(2,2,2)) )

l = apply(sR,i -> i / (j -> dim last j))
assert( (l) == {{1},{18},{52},{60},{24,10},{12},{3}} )

n = 5;
S = schurRing(QQ,s,n,GroupActing => "Sn");
rep = s_n + s_{n-1,1};
M = {s_n}
sR = schurResolution(rep,M,DegreeLimit => n)
assert( (last sR#2) == (2,s_(4,1)+s_(3,1,1)) )
assert( (first sR#3) == (3,s_(3,1,1)+s_(2,1,1,1)) )
assert( (last last sR) == (5,s_(1,1,1,1,1)) )

l = apply(sR,i -> i / (j -> dim last j))
assert( (l) == {{1},{5},{10},{10},{5},{1}} )

M = {s_n} | splice{n:rep};
sR = schurResolution(rep,M)    
assert( (last sR#2) == (3,s_(4,1)+s_(3,2)+s_(3,1,1)+s_(2,2,1)) )
assert( (first sR#3) == (4,s_(3,1,1)+s_(2,2,1)+s_(2,1,1,1)) )
assert( (last last sR) == (5,s_(2,1,1,1)) )

l = apply(sR,i -> i / (j -> dim last j))
assert( (l) == {{1},{10},{20},{15},{4}} )
///

---------------------------
-- end test schurResolution
---------------------------


end

restart
loadPackage"SchurRings"

d = 11

mul = method()
mul(List,List,List,List) := (lam,mu,nu,del) ->
(
     	 m := sum lam;
	 (llam,lmu,lnu,ldel) := (lam,mu,nu,del);
     	 if not llam#?1 then llam = llam | {0};
     	 if not lmu#?1 then lmu = lmu | {0};
     	 if not lnu#?1 then lnu = lnu | {0};
     	 if not ldel#?1 then ldel = ldel | {0};	 	 	 
	 e := llam#1 + lmu#1 + lnu#1 + ldel#1;
	 f := max(llam#1, lmu#1, lnu#1, ldel#1);
      	 local rez;
	 if e>=m-1 then
    	   (
         	rez = m//2 - f + 1;
	  	if e%2 == 1 and m%2 == 0 then rez = rez - 1;
		)
     	 else
          (
	  rez = (e+1)//2 - f + 1;
          if e%2 == 1 then rez = rez - 1;
	  );
         max(0,rez)
     )

T_1 = schurRing(QQ,t1,2);

l = {(T_1)_{1}}
for i from 2 to 4 do
(
    T_i = schurRing(T_(i-1),value concatenate("t",toString i),2);
    l = l | {(T_i)_{1}};
)
rep = product l

mods = new MutableList;
mods#0 = 1_(T_4)

for i from 1 to d do
(
       pars = {{i}} | apply(splice{1..(i//2)},j->{i-j,j});
       mods#i = sum (for p in (toList (set pars)^**4) list
       mul(p#0,p#1,p#2,p#3)*(T_1)_(p#0)*(T_2)_(p#1)*(T_3)_(p#2)*(T_4)_(p#3));
)
M = toList mods		  


resol = schurResolution(rep,M,DegreeLimit => d)
resol/(i->(sum apply(i,j->dim(last j))))				    						  											    																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																													 											    

restart
uninstallPackage"SchurRings"
installPackage"SchurRings"
check SchurRings
viewHelp SchurRings

restart
debug loadPackage"SchurRings"

S = schurRing(QQ,s,3)
T = schurRing(S,t,4)
rep = s_1 * t_1
symmetricPower(8,rep)

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages PACKAGES=SchurRings pre-install"
-- End:
