-- -*- coding: utf-8 -*-
-----------------------------------------------------------------------
-- Copyright 2008--2016 Graham Denham, Gregory G. Smith
--
-- You may redistribute this program under the terms of the GNU General
-- Public License as published by the Free Software Foundation, either
-- version 2 or the License, or any later version.
-----------------------------------------------------------------------

-- Jan 27 2011: hyperplane arrangements package;
-- Graham Denham and Greg Smith, with
-- thanks to Sorin Popescu for the Orlik-Solomon code
--
-- release 0.9, March 2016: fixed base-field bug in Orlik-Solomon code 
--
-- release 0.8:
-- * previously, all arrangements were assumed to be central:
--   some code also worked for affine arrangements, and some
--   broke.  Now we allow affine arrangements where possible,
--   by making central arrangements a new type.
-- * removed some undocumented functions.
--
-- release 0.8-: 
-- corrects a bug that affected multiplier ideals, added log-canonical
-- threshold
--
-- release 0.7:
-- more canned arrangements
-- 
-- new in release 0.6: multiplier ideals; bug fixes (in particular
-- involving empty arrangements); some caching; a "circuits" method;
-- test for (Lie algebra) decomposability; a random arrangement;
-- a hash table full of popular "canned" arrangements.
-- 
-- 6 Jan 09: fixed bug in changeRing and errors in TEST section
-- 1 Jun 09: fixed inconsistency in multIdeal 

newPackage(
     "HyperplaneArrangements",
     Version => "0.9",
     Date => "29 January 2011",
     Authors => {
	  {Name => "Graham Denham", HomePage => "http://www.math.uwo.ca/~gdenham/"},
	  {Name => "Gregory G. Smith", Email => "ggsmith@mast.queensu.ca", HomePage => "http://www.mast.queensu.ca/~ggsmith"}
	  },
     Headline => "hyperplane arrangements",
     Keywords => {"Commutative Algebra"},
     DebuggingMode => false
     )

export {"Arrangement", "arrangement", "arrangementLibrary", "CentralArrangement",
     "deCone", "deletion", "orlikSolomon", "orlikTerao", "HypAtInfinity",
     "NaiveAlgorithm", "typeA", "typeB", "typeD", "graphic", "Flat", "flat", "flats",
     "circuits", "tolist", "closure", "meet", "vee", "subArrangement", "changeRing",
     "restriction", "arrangementSum", "EPY", "der", "HS", "isDecomposable", "isCentral", 
     "multIdeal", "lct", "randomArrangement", "Classic"}

-- these are already defined: compress, trim, coefficients,
-- euler, poincare, cone, rank, ring, matrix

Arrangement = new Type of HashTable
Arrangement.synonym = "hyperplane arrangement"
Arrangement.GlobalAssignHook = globalAssignFunction
Arrangement.GlobalReleaseHook = globalReleaseFunction
Arrangement#{Standard,AfterPrint} = A -> (
     << endl;
     << concatenate(interpreterDepth:"o") << lineNumber << " : Hyperplane Arrangement "
     << endl;
     )

CentralArrangement = new Type of Arrangement
CentralArrangement.synonym = "central hyperplane arrangement"
CentralArrangement.GlobalAssignHook = globalAssignFunction
CentralArrangement.GlobalReleaseHook = globalReleaseFunction

debug Core
-- we'll have a better way to do this later
net Arrangement := A -> if hasAttribute(A,ReverseDictionary) then toString getAttribute(A,ReverseDictionary) else net expression A
dictionaryPath = delete(Core#"private dictionary", dictionaryPath)

protect hyperplanes
net Arrangement := A -> net expression A
expression Arrangement := A -> new RowExpression from { A.hyperplanes }
describe Arrangement := A -> net A.hyperplanes

arrangement = method(TypicalValue => Arrangement)
arrangement (List,Ring) := Arrangement => (L,R) -> (
     if #L > 0 and ring L#0 =!= R then (
	  f := map(R, ring L#0);
	  A := L / f)
     else A = L;
     central := true;
     if #L > 0 then central = fold( (p,q) -> p and q, isHomogeneous\L);
     central = central and isHomogeneous R;
     data := {
	  symbol ring => R,
	  symbol hyperplanes => A,
	  symbol cache => new CacheTable
	  };
     if central then 
          new CentralArrangement from data else
     	  new Arrangement from data)

arrangement List := Arrangement => L -> (
     if #L == 0 then error "Empty arrangement has no default ring"
     else arrangement(L, ring L#0))

arrangement (Arrangement,Ring) := Arrangement => (A,R) -> arrangement(A.hyperplanes,R)

arrangement (Matrix,Ring) := Arrangement => (M,R) -> (
     arrangement(flatten entries((vars R)*M), R));

arrangement Matrix := Arrangement => M -> (
     k := ring M_(0,0);
     x := symbol x;
     n := numrows M;
     R := k[x_1..x_n];
     arrangement(M,R));

-- arrangement from a polynomial: if it's unreduced, have multiplicities

arrangement RingElement := Arrangement => Q -> (
     l := select(toList factor Q, p -> 0 < (degree p#0)_0);  -- kill scalar
     arrangement (flatten (l/(p->toList(p#1:p#0)))));
         
-- look up a canned arrangement

arrangement String := Arrangement => name -> (
     if not arrangementLibrary#?name then error "No information available for ", name;
     k := ring arrangementLibrary#name;
     if k === ZZ then k = QQ;
     arrangement(k**arrangementLibrary#name));

arrangement (String, PolynomialRing) := Arrangement => (name,R) -> (
     if not arrangementLibrary#?name then error "No information available for ", name;
     arrangement(arrangementLibrary#name,R));

arrangement (String, Ring) := Arrangement => (name,k) -> (
     if not arrangementLibrary#?name then error("No information available for ", name);
     arrangement(k**arrangementLibrary#name));

-- here are some canned arrangements that are convenient to have
-- on hand

arrangementLibrary = hashTable({
     "braid" => transpose matrix {{1,0,0},{0,1,0},{0,0,1},{1,-1,0},{1,0,-1},{0,1,-1}},
     "X2" => transpose matrix {{1,0,0},{0,1,0},{0,0,1},{0,1,-1},{1,0,-1},
	  {1,1,0},{1,1,-2}},
     "X3" => transpose matrix {{1,0,0},{0,1,0},{0,0,1},{1,1,0},{1,0,1},{0,1,1}},
     "Pappus" => transpose matrix {{1,0,0},{0,1,0},{0,0,1},{1,-1,0},{0,1,-1},
	  {1,-1,-1},{2,1,1},{2,1,-1},{2,-5,1}},
     "(9_3)_2" => transpose matrix {{1,0,0},{0,1,0},{0,0,1},{1,1,0},{0,1,1},{1,0,3},
	  {1,2,1},{1,2,3},{4,6,6}},
     "nonFano" => transpose matrix {{1,0,0},{0,1,0},{0,0,1},{0,1,-1},{1,0,-1},
	  {1,-1,0},{1,1,-1}},
     "MacLane" => (ZZ/31627)**transpose matrix{{1,0,0},{0,1,0},{0,0,1},{1,-1,0},
	  {1,0,-1},{0,1,25207},{1,25207,-1},{1,25207,6419}},
     "Hessian" => (ZZ/31627)**transpose matrix{{1,0,0},{0,1,0},{0,0,1},
	  {1,1,1},{1,1,6419},{1,1,25207},{1,6419,1},{1,6419,6419},{1,6419,25207},
	  {1,25207,1},{1,25207,6419},{1,25207,25207}},
     "Ziegler1" => transpose matrix {{1,0,0},{0,1,0},{0,0,1},{1,1,1},{2,1,1},
	  {2,3,1},{2,3,4},{3,0,5},{3,4,5}},
     "Ziegler2" => transpose matrix {{1,0,0},{0,1,0},{0,0,1},{1,1,1},{2,1,1},{2,3,1}
	  ,{2,3,4},{1,0,3},{1,2,3}},
     "prism" => transpose matrix {{1,0,0,0},{0,1,0,0},{0,0,1,0},{0,0,0,1},{1,1,0,1},
	  {1,0,1,1}},
     "notTame" => transpose matrix {{1,0,0,0},{0,1,0,0},{0,0,1,0},{0,0,0,1},
	  {1,1,0,0},{1,0,1,0},{1,0,0,1},{0,1,1,0},{0,1,0,1},{0,0,1,1},
          {1,1,1,0},{1,1,0,1},{1,0,1,1},{0,1,1,1},{1,1,1,1}},
     "bracelet" => matrix {{1,0,0,1,0,0,1,1,0}, {0,1,0,0,1,0,1,0,1}, 
     	  {0,0,1,0,0,1,0,1,1}, {0,0,0,1,1,1,1,1,1}},
     "Desargues" => transpose matrix {{1, 0, 0}, {0, 1, 0}, {0, 0, 1}, {1, 1, 1}, {2, 0, -3}, {2, 1, -3}, 
	  {-3, -2, 2}, {1, 2, 1}, {3, 2, 1}, {2, 1, 0}}
     })

ring Arrangement := Ring => A -> A.ring

tolist = method(TypicalValue=>List);  -- prefer to overload toList; see join
tolist Arrangement := List => A -> A.hyperplanes

matrix Arrangement := Matrix => options -> A -> matrix {tolist A}

coefficients Arrangement := Matrix => options -> A -> (
     if (tolist A == {}) then 0 else jacobian matrix A)

rank Arrangement := A -> 
     if (tolist A == {}) then 0 else (
	  Ap := prune A;
	  k := coefficientRing ring Ap;
	  rank lift(coefficients Ap, k));

normal := h -> (
     h/leadCoefficient h);  -- representative of functional, mod scalars

-- arrangements may usually be taken to be central without loss of generality:
-- however, sometimes noncentral arrangements are convenient

isCentral = method(TypicalValue => Boolean);
isCentral Arrangement := Boolean => A -> instance(A, CentralArrangement)

-- arrangements may sensibly be defined over quotients of polynomial
-- rings by affine-linear ideals.  However, sometimes this is a pain,
-- so we provide

prune Arrangement := Arrangement => options -> A -> (
     R := ring A;
     if not instance(R,PolynomialRing) then (
	  S := prune R;
	  f := R.minimalPresentationMap;
	  arrangement(f \ tolist A, S)) 
     else A);

-- reduce an arrangement with possibly repeated hyperplanes to a 
-- simple arrangement.  Cache the simple arrangement and multipliticies.

protect m
protect simple

trim Arrangement := Arrangement => options -> A ->  (
     if A.cache.?simple then return(A.cache.simple);
     if (tolist A == {}) then (
	  A.cache.simple = A; A.cache.m = {}; A)
     else (
	  count := new MutableHashTable;
	  scan(tolist A, h -> (
		    if h != 0 then (
		    	 if not count#?(normal h) then count#(normal h) = 0;
			 count#(normal h) = 1+count#(normal h))));
	  (L,m) := (keys(count),values(count));
	  A' := arrangement(L, ring A);
 	  A.cache.m = m; 
	  A.cache.simple = A'));

-- remove degenerate hyperplanes arising in restriction

compress Arrangement := Arrangement => A -> 
     if (A.hyperplanes == {}) then A else (
	  L := select(A.hyperplanes, h -> first degree(h) == 1);
	  arrangement(L, ring A))

dual CentralArrangement := CentralArrangement => A -> (
     if (tolist A == {}) then error "dual expects a nonempty arrangement";
     C := transpose gens kernel coefficients A; 
     R := ring A;
     f := map(coefficientRing R, R);
     arrangement(f C));
        
-- equality testing

Arrangement == Arrangement := (A,B) -> (
     if (A.ring === B.ring) then ((tolist A) == (tolist B))
     	  else false)
     
-- deletion; restriction is a special case of res. to a flat, so comes 
-- later

deletion = method(TypicalValue => Arrangement)
deletion (Arrangement,RingElement) := Arrangement => (A,h) -> (
     arrangement(select(A.hyperplanes,i->(i != h)), ring A))

-- a non-central arrangement may be defined over an inhomogeneous 
-- quotient of a polynomial ring, so we need to prune it

cone (Arrangement,RingElement) := CentralArrangement => (A,h) -> (
     prune arrangement ((apply(A.hyperplanes,i->homogenize(i,h))) | {h}));

cone (Arrangement,Symbol) := CentralArrangement => (A,h) -> (
     R := ring A;
     S := (coefficientRing R)[h];
     T := tensor(R,S,Degrees=>toList ((numgens(R)+numgens(S)):1));
     f := map(T,S);
     cone (arrangement(A,T),f S_0));

deCone = method(TypicalValue => Arrangement)
deCone (CentralArrangement,RingElement) := Arrangement => (A,h) -> (
     A' := deletion(A,h);
     arrangement(A', (ring A')/ideal(h-1)));

deCone (CentralArrangement,ZZ) := Arrangement => (A,i) -> (
     h := (tolist A)_i;
     deCone(A,h));

partial := m -> (
     E := ring m;
     sum first entries compress diff(vars E,m));

monomialSubIdeal := I -> (  -- note: add options (See SP's code)
     R := ring I;
     K := I;
     J := ideal(1_R);
     while (not isMonomialIdeal K) do (
	  J = ideal leadTerm gens gb K;
	  K = intersect(I,J));
     ideal mingens K);


-- If orlikSolomon is given a central arrangement, 
-- it returns an ideal I with OS = E/I, where E is the ring of I
-- and OS is the (central) Orlik-Solomon algebra.
-- 
-- If the input is not central, we cone (homogenize) and then
-- dehomogenize.
--
-- in the central case, the same ideal defines the cohomology 
-- ring of the projective complement, but in a subalgebra of E.
--
-- Since we can't construct this in M2, the option Projective 
-- returns a larger ideal I' so that E/I' is the cohomology ring
-- of the projective complement, written in coordinates that put
-- a hyperplane H_j at infinity.
--
-- not clear this is the best...
--
-- we also expect this method to cache the circuits of A, as a 
-- list of exterior monomials, since this calculation is expensive.
-- bug fix in June 2013: circuits are defined over the coefficient ring
-- of the arrangement.  

orlikSolomon = method(TypicalValue => Ideal, 
                      Options => {Projective => false, HypAtInfinity => 0});

orlikSolomon (CentralArrangement, PolynomialRing) := Ideal => o -> (A,E) -> (
     n := #A.hyperplanes;
     if n == 0 then (
	  if o.Projective then error "Empty projective arrangement is not allowed."
	  else return ideal(0_E)); -- empty affine arrangement is contractible.
     e := symbol e;
     nativeField := coefficientRing ring A;
     if not A.cache.?circuits then (
          A.cache.circuits = new MutableHashTable;
          Ep := nativeField[e_1..e_n,SkewCommutative=>true];
     	  C := substitute(syz coefficients A,Ep);
     	  M := monomialSubIdeal( ideal( (vars Ep) * C));
	  A.cache.circuits = (Ep, flatten entries gens M));
     f := map(E,A.cache.circuits_0,vars E); -- note: map changes coefficient ring
     I := ideal append( apply(A.cache.circuits_1/f, r -> partial r),0_E);
     if o.Projective then trim I+ideal(E_(o.HypAtInfinity)) else trim I);

-- if the arrangement is not central, cone first, then project back

orlikSolomon (Arrangement, PolynomialRing) := Ideal => o -> (A,E) -> (
     h := symbol h;
     e := symbol e;
     cA := cone(A,h);
     k := coefficientRing E;
     cE := E**k[e,SkewCommutative=>true];
     proj := map(E,cE);
     proj orlikSolomon (cA, cE));

orlikSolomon (Arrangement,Symbol) := Ideal => o -> (A,e) -> (
     n := #A.hyperplanes;
     E := coefficientRing(ring A)[e_1..e_n,SkewCommutative=>true];
     orlikSolomon(A,E,o));

-- one can just specify a coefficient ring
-- note that this affects the arrangement: is this the desired behavior?

orlikSolomon (Arrangement,Ring) := Ideal => o -> (A,k) -> (
     e := symbol e;
     n := #A.hyperplanes;
     E := k[e_1..e_n,SkewCommutative=>true];
     orlikSolomon(A,E,o));

orlikSolomon (Arrangement) := Ideal => o -> (A) -> (
     e := symbol e;
     orlikSolomon(A,e,o));

--  can't forward options, since existing method doesn't have options.
poincare (Arrangement) := RingElement => A -> (
     I := orlikSolomon A;
     numerator reduceHilbert hilbertSeries ((ring I)/I));

-- Euler characteristic of (proj) complement
-- complement of empty arrangement is CP^{n-1}

euler (Arrangement) := ZZ => A -> (
     if #tolist A == 0 then dim ring A else (
     	  I := orlikSolomon(A,Projective=>true);
     	  f := numerator reduceHilbert hilbertSeries ((ring I)/I);
     	  sub(f,{(ring f)_0 => -1})));

-- euler(Flat) coming later

-- some constructions of Coxeter type
-- default coefficients are now QQ

typeA = method(TypicalValue => Arrangement)
typeA (ZZ,PolynomialRing) := Arrangement => (n,R) -> (
     arrangement flatten apply(n,i->apply(toList(i+1..n),j->R_i-R_j)));

typeA (ZZ,Ring) := Arrangement => (n,k) -> (
     x := symbol x;
     R := k[x_1..x_(n+1)];
     typeA(n,R));

typeA (ZZ) := Arrangement => n -> typeA(n,QQ);

typeD = method(TypicalValue => Arrangement)
typeD (ZZ,PolynomialRing) := Arrangement => (n,R) -> (
     arrangement flatten apply(n-1,i->(
	         flatten apply(toList(i+1..n-1),(j->{R_i-R_j,R_i+R_j})))));

typeD (ZZ,Ring) := Arrangement => (n,k) -> (
     x := symbol x;
     R := k[x_1..x_n];
     typeD(n,R));

typeD (ZZ) := Arrangement => n -> typeD(n,QQ);

typeB = method(TypicalValue => Arrangement)
typeB (ZZ,PolynomialRing) := Arrangement => (n,R) -> (
     arrangement ( apply(n,i->R_i) | (tolist(typeD(n,R)))));

typeB (ZZ,Ring) := Arrangement => (n,k) -> (
     x := symbol x;
     R := k[x_1..x_n];
     typeB(n,R));

typeB (ZZ) := Arrangement => n -> typeB(n,QQ);

-- construct a graphic arrangement, from a graph given by a list
-- of edges.  Assume vertices are integers 1..n

graphic = method(TypicalValue => Arrangement)
graphic (List,PolynomialRing) := Arrangement => (G,R) -> 
     arrangement (G/(e->(R_(e_1-1)-R_(e_0-1))))
     
graphic (List,Ring) := Arrangement => (G,k) -> (
     n := max flatten G;
     x := symbol x;
     R := k[x_1..x_n];
     graphic(G,R));

graphic (List) := Arrangement => G -> (
     graphic(G,QQ));

-- return a random arrangement of n hyperplanes in l-space.  For large enough 
-- N, this will tend to be the uniform matroid.

randomArrangement = method(TypicalValue => Arrangement)
randomArrangement (ZZ,ZZ,ZZ) := Arrangement => (n,l,N) -> (
     m := QQ**matrix randomMutableMatrix(l,n,0.,N);
     arrangement m);     

-- intersection lattice and flats:

Flat = new Type of HashTable
Flat.synonym = "intersection of hyperplane(s)"
Flat#{Standard,AfterPrint} = F -> (
     << endl;
     <<  concatenate(interpreterDepth:"o") << lineNumber << " : Flat of " << F.arrangement
     << endl;
     )

net Flat := F -> net F.flat
expression Flat := (F) -> new Holder from { F.flat }
 
flat = method(TypicalValue => Flat)

flat (Arrangement,List) := Flat => (A,F) ->
new Flat from {
     symbol flat => F,
     symbol arrangement => A,
     symbol cache => new CacheTable
}

arrangement Flat := Arrangement => F -> (   -- get arrangement of a flat F
     F.arrangement);

euler (Flat) := ZZ => F -> (
     euler subArrangement F);  -- aka beta invariant

tolist Flat := List => F -> (
     F.flat);

-- test equality

Flat == Flat := (X,Y) -> (
     if (arrangement X == arrangement Y) then ((tolist X) == (tolist Y))
          else false)
     
closure = method(TypicalValue => Flat)  
closure (Arrangement,Ideal) := Flat => (A,I) -> (
     flat(A,positions(A.hyperplanes,h -> h % gb I == 0)));

closure (Arrangement,List) := Flat => (A,S) -> (
     closure (A,ideal (A.hyperplanes_S|{0_(ring A)})));   -- ugly hack for empty list

closure (Arrangement,Arrangement) := Flat => (A,B) -> (
     closure (A,ideal B));

-- lattice operations  

meet = method(TypicalValue => Flat)
meet (Flat,Flat) := Flat => (F,G) -> (
     A := arrangement F;
     if (A =!= arrangement G) then error "need the same arrangement"; 
     flat(A,select((tolist F),i->member(i,tolist G))));

-- join would be better; see tolist

vee = method(TypicalValue => Flat)
vee (Flat,Flat) := Flat => (F,G) -> (
     A := arrangement F;
     if (A =!= arrangement G) then error "need the same arrangement"; 
     closure(A,(tolist F)|(tolist G)));

Flat | Flat := Flat => vee  
Flat ^ Flat := Flat => meet  -- ooh, cool.  But note L_1^L_2 isn't L_1^(L_2) !

subArrangement = method(TypicalValue => Arrangement)
subArrangement (Flat) := Arrangement => (F) -> (
     A := arrangement F;
     arrangement(A.hyperplanes_(tolist F), ring A));

-- the next version is redundant, but I'm putting it in 
-- in case users want to use the usual notation

subArrangement (Arrangement,Flat) := Arrangement => (A,F) -> (
     if (A =!= arrangement F) then error "not a flat of the arrangement";
     subArrangement F);

Arrangement _ Flat := Arrangement => subArrangement

-- restriction will return a (i) multiarrangement with (ii) natural
-- coordinate ring; maybe not what everyone expects
-- empty flat needs special treatment; better style possible here, Greg?

restriction = method(TypicalValue => Arrangement)
restriction (Flat) := Arrangement => (F) -> (
     A := arrangement F;
     R := ring A;
     compress arrangement(A,R/(ideal ((tolist A)_(tolist F)|{0_R}))));

restriction (Arrangement,Flat) := Arrangement => (A,F) -> (
     if (A =!= arrangement F) then error "not a flat of the arrangement";
     restriction F);

restriction (Arrangement,ZZ) := Arrangement => (A,i) -> (
     restriction(A, flat(A, {i})));

Arrangement ^ Flat := Arrangement => restriction

-- in the sense of matroid contraction
-- needs some error checking?

restriction (Arrangement,RingElement) := Arrangement => (A,h) -> (
     compress arrangement(A,(ring A)/(ideal h)));

restriction (Arrangement,Ideal) := Arrangement => (A,I) -> (
     compress arrangement(A,(ring A)/I));

rank (Flat) := ZZ => F -> (rank subArrangement F);

flats = method(TypicalValue => List)
flats (ZZ,Arrangement) := List => (j,A) -> (
     I := orlikSolomon A;
     OS := (ring I)/I;
     L := flatten entries basis(j,OS);
     unique(L/indices/(S->closure(A,S))));

flats (Arrangement) := List => A -> (
     apply(1+rank A,j->flats(j,A)));

-- return list of indices of hyperplanes in minimal dependent sets

circuits = method(TypicalValue => List) 
circuits Arrangement := List => A -> (
     if #tolist A == 0 then return({}); -- empty arrangement is special
     k := coefficientRing(ring A);
     if not A.cache.?circuits then orlikSolomon A;
-- turn each monomial in list into its set of indices
     (m -> indices m)\A.cache.circuits_1);     
     
-- direct sum of two arrangements  ( can't overload "directSum" or "tensor")

arrangementSum = method(TypicalValue => Arrangement)
arrangementSum (Arrangement, Arrangement) := Arrangement => (A,B) -> (
     R := ring A; S := ring B;
     RS := tensor(R,S,Degrees => toList ((numgens(R)+numgens(S)):1));
     f := map(RS,R); g := map(RS,S);
     arrangement ((tolist A)/f|(tolist B)/g, RS));

-- change of rings shares an abbreviation:

changeRing = method(TypicalValue => Arrangement)
changeRing (Arrangement, Ring) := Arrangement => (A, k) -> (
     R := ring A;
     f := map(R**k, R);
     arrangement ((tolist A)/f, R**k));

Arrangement ** Ring := Arrangement => changeRing
Arrangement ** Arrangement := Arrangement => arrangementSum

-- check if arrangement is decomposable in the sense of Papadima-Suciu
-- we need to distinguish between the coefficients in A and
-- the coefficients for I

isDecomposable = method(TypicalValue => Boolean)

isDecomposable (CentralArrangement,Ring) := Boolean => (A,k) -> (
     I := orlikSolomon (A,k);
     b := betti res(coker vars ((ring I)/I), LengthLimit=>3);
     phi3 := 3*b_(3,{3},3)-3*b_(1,{1},1)*b_(2,{2},2)+b_(1,{1},1)^3-b_(1,{1},1);
     multiplicities := apply(flats(2,A),i->length tolist i);
     sum(multiplicities,m->m*(2-3*m+m^2)) == phi3);

isDecomposable (CentralArrangement) := Boolean => A -> (
     k := coefficientRing(ring A);
     isDecomposable(A,k));

symExt = (m,R) ->(
     if (not(isPolynomialRing(R))) then error "expected a polynomial ring or an exterior algebra";
     if (numgens R != numgens ring m) then error "the given ring has a wrong number of variables";
     ev := map(R,ring m,vars R);
     mt := transpose jacobian m;
     jn := gens kernel mt;
     q  := vars(ring m)**id_(target m);
     n  := ev(q*jn))

-- EPY module, formerly called FA

EPY = method(TypicalValue => Module);

EPY (Ideal,PolynomialRing) := Module => (j, R) -> (
     modT := (ring j)^1/(j*(ring j^1));
     F := res(prune modT, LengthLimit=>3);
     g := transpose F.dd_2;
     G := res(coker g,LengthLimit=>4);
     FA := coker symExt(G.dd_4, R);
     d := first flatten degrees cover FA;
     FA**(ring FA)^{d});  -- GD: I want this to be generated in degree 0

EPY (Ideal) := Module => (j) -> (
     n := numgens ring j;
     f := symbol f;
     X := getSymbol "X";
     R := coefficientRing(ring j)[X_1..X_n];
     EPY(j, R));

EPY (Arrangement) := Module => A -> EPY orlikSolomon A;
EPY (Arrangement,PolynomialRing) := Module => (A,R) -> EPY(orlikSolomon A, R);

-- the Orlik-Terao algebra

orlikTeraoV1 := (A,S) -> (     
     n := #tolist A; R := ring A;
     if n == 0 then return ideal(0_S);
     if (numgens S != n) then error "the given ring has a wrong number of variables";
     hyps := tolist A;
     Q := product hyps;
     quotients := hyps/(h->Q//h);
     trim ker map(R,S, quotients));

-- construct the relation associated with a circuit

OTreln := (c, M, S) -> (  -- circuit, coeffs, ring of definition
     v := gens ker M_c;
     f := map(S, ring v);
     P := product(c/(i->S_i));  -- monomial
     (matrix {c/(i->P//S_i)} * f v)_(0,0));

-- this older version builds the ideal "manually": definitely slower, so kept
-- only to add a test.
     
orlikTeraoV2 := (A,S) -> (     
     n := #tolist A;
     if n == 0 then return ideal(0_S);
     if (numgens S != n) then error "the given ring has a wrong number of variables";
     vlist := flatten entries vars S;
     M := coefficients A;
     trim ideal(circuits A/(c -> OTreln(c,M,S))));
     
orlikTerao = method(TypicalValue => Ideal, Options => {NaiveAlgorithm => false});
orlikTerao (CentralArrangement,PolynomialRing) := Ideal => o -> (A,S) -> (
     if o.NaiveAlgorithm then orlikTeraoV2(A,S) else orlikTeraoV1(A,S));
     
orlikTerao (CentralArrangement,Symbol) := Ideal => o -> (A,y) -> (
     n := #A.hyperplanes;
     S := coefficientRing(ring A)[y_1..y_n];
     orlikTerao(A,S,o));

orlikTerao (CentralArrangement) := Ideal => o -> A -> (          
     y := symbol y; 
     orlikTerao(A,y,o));

-- add exceptionals, complex refl groups?

-- module of derivations;  needs adjustment if ring of A is not polynomial.
-- returns module of derivations

der = method(TypicalValue => Matrix, Options => {Strategy => null});
der (CentralArrangement) := Matrix => o -> A -> (
     Ap := prune A;  -- ring of A needs to be polynomial
     if o.Strategy === Classic then der1(Ap) else (
	  if not Ap.cache.?simple then trim(Ap);
     	  der2(Ap.cache.simple, Ap.cache.m)));

der (CentralArrangement,List) := Matrix => o -> (A,m) -> (
     der2(prune A,m));   -- it's a multiarrangement if multiplicities supplied

-- Note: no removal of degree 0 part.
     
der1 = A -> (
     Q := product tolist A;   -- defining polynomial
     J := jacobian ideal Q;
     m := gens ker map(transpose J | -Q, Degree => -1);
     l := rank A;
     submatrix(m,0..(l-1),));

-- simple arrangement with a vector of multiplicities

der2 = (A,m) -> (
     hyps := tolist A;
     R := ring A;
     n := #hyps;
     l := numgens R;
     P := transpose coefficients A;
     D := diagonalMatrix apply(n, i-> hyps_i^(m_i));
     proj := map(R^-m,R^(n+l),map(R^n,R^l,0) | map(R^n,R^n,1));
     proj*gens ker(map(P, Degree=>-1) | D));

-- compute multiplier ideals of an arrangement, via theorems of 
-- Mustata and Teitler

weight := (F,m) -> (
     sum((tolist F)/(i->m_i)));

protect multipliers
protect irreds

multIdeal = method(TypicalValue => Ideal)
-- it's expensive to recompute the list of irreducible flats, 
-- as well as intersections of ideals.  So we cache a hash table
-- whose keys are the lists of exponents on each ideal, and whose
-- values are the intersection.

multIdeal (QQ,CentralArrangement,List) := Ideal => (s,A,m) -> (
     if (#tolist A != #m) then error "expected one weight for each hyperplane";
     R := ring A;
     if not A.cache.?irreds then
	  A.cache.irreds = select(flatten drop(flats(A),1), F->(0 != euler F));
     exps := A.cache.irreds/(F->max(0,floor(s*weight(F,m))-rank(F)+1));
     if not A.cache.?multipliers then A.cache.multipliers = new MutableHashTable;
     if not A.cache.multipliers#?exps then (
	  ideals := A.cache.irreds/(F-> trim ideal tolist (A_F));
	  A.cache.multipliers#exps = intersect apply(#exps, i->(ideals_i)^(exps_i)))
     else
     	  A.cache.multipliers#exps);

multIdeal (QQ,CentralArrangement) := Ideal => (s,A) -> (
     if not A.cache.?simple then trim A;
     multIdeal(s,A.cache.simple,A.cache.m));

-- numeric argument might be an integer:
multIdeal (ZZ,CentralArrangement) := Ideal => (s,A) -> multIdeal(s*1/1, A);
multIdeal (ZZ,CentralArrangement,List) := Ideal => (s,A,m) -> multIdeal(s*1/1, A, m);

-- log-canonical threshold:
-- use the observation that the jumping numbers must be rationals with
-- denominators that divide the weight of one or more flats.

lct = method(TypicalValue => QQ);

lct CentralArrangement := QQ => A -> (
     I0 := multIdeal(0,A);  -- cache the irreducibles, make A a multiarrangement
     irreds := A.cache.simple.cache.irreds;
     N := lcm(irreds/(F->weight(F,A.cache.m)));
     s := 1;
     while I0 == multIdeal(s/N,A) do s = s+1;
     s/N);     

-- critical set ideal: internal use only.

HS = i -> reduceHilbert hilbertSeries i;

beginDocumentation()

undocumented {HS}

document { 
     Key => HyperplaneArrangements,
     Headline => "hyperplane arrangements",
     EM "HyperplaneArrangement", " is a package for manipulating
     hyperplane arrangements.",
     PARA{},
          "A hyperplane arrangement is a finite set of hyperplanes in an
     affine or projective space.  In this package, an arrangement is 
     expressed as a list of (linear) defining equations for the hyperplanes.
     The tools provided allow the user to create new arrangements from
     old, and to compute various algebraic invariants of arrangements."
     }

document {  Key => Arrangement,
     Headline => "class of hyperplane arrangements",
     PARA{},
     "A hyperplane is an affine-linear subspace of codimension one.  An
     arrangement is a finite set of hyperplanes.",
     }

document {  Key => CentralArrangement,
     Headline => "class of central hyperplane arrangements",
     PARA{},
     "A central arrangement is a finite set of linear hyperplanes."
     }

document { 
     Key => {arrangement, (arrangement,List), (arrangement,List,Ring),
     	  (arrangement,Arrangement,Ring), (arrangement,Matrix), (arrangement,Matrix,Ring),
	  (arrangement,RingElement)},
     Headline => "create a hyperplane arrangement",
     Usage => "arrangement(L,R) or arrangement(M) or arrangement(M,R) or
     arrangement Q",
     Inputs => {
	  "L" => {"a list of affine-linear equations in the ring ", TT "R"},
	  "R" => {"a polynomial ring or linear quotient of a
	       polynomial ring"},	  
	  "M" => {"a matrix whose columns represent linear forms defining
	       hyperplanes"},
	  "Q" => {"a product of linear forms"},
          },
     Outputs => {
	  Arrangement => {"the hyperplane arrangement determined by ",
	       TT "L", " and ", TT "R"},
          },
     "A hyperplane is an affine-linear subspace of codimension one.  An
     arrangement is a finite set of hyperplanes.  If each hyperplane
     contains the origin, the arrangement is a ", 
     TO2(CentralArrangement, "central arrangement"),".",
     PARA{},
     "Probably the best-known hyperplane arrangement is the braid
     arrangement consisting of all the diagonal hyperplanes.  In
     4-space, it is constructed as follows: ",
     EXAMPLE {
          "S = ZZ[w,x,y,z];",
	  "A3 = arrangement {w-x,w-y,w-z,x-y,x-z,y-z}",
	  "describe A3",
	  },
     "If we project along onto a subspace, then we obtain an essential
     arrangement:",
     EXAMPLE {     
	  "R = S/ideal(w+x+y+z)",
	  "A3' = arrangement({w-x,w-y,w-z,x-y,x-z,y-z},R)",
	  "describe A3'",
	  },          
     "The trivial arrangement has no equations.",
     EXAMPLE {     
	  "trivial = arrangement({},S)",
	  "describe trivial",
	  "ring trivial",
	  },
     EXAMPLE {
	  "use S;",
	  "arrangement (x^2*y^2*(x^2-y^2)*(x^2-z^2))",
	  },
     Caveat => {"If the elements of ", TT "L", " are not ",
	  TO2(RingElement, "ring elements"), " in ", TT "R", ", then
	  the induced identity map is used to map them from ", 
	  TT "ring L#0", " into ", TT "R", ".",
	  PARA{},
          " If ", TT "arrangement Q", " is used, the order of the
	  factors is determined internally."},
     SeeAlso => {HyperplaneArrangements,(arrangement,String,PolynomialRing)}
     }

document { 
     Key => {(arrangement,String,PolynomialRing)},
     Headline => "look up a built-in hyperplane arrangement",
     Usage => "arrangement(s) or arrangement(s,R) or arrangement(s,k)",
     Inputs => {
	  "s" => String => "the name of a built-in arrangement",
  	  "R" => PolynomialRing => "an optional coordinate ring for the 
  	  arrangement",
	  },
     Outputs => {
	  Arrangement => {"the hyperplane arrangement named ", TT "s", "."}
	  },
     "The built-in arrangements are stored in a global ", TO HashTable, 
     " called ", TT "arrangementLibrary", ".  Accordingly, the user can
     see what arrangements are available by examining the keys:",
     EXAMPLE lines ///
	  keys arrangementLibrary
	  R = QQ[x,y,z];
	  A = arrangement("Pappus",R)
	  poincare A
	  isDecomposable A
     	  A = arrangement("prism", ZZ/101) -- can also specify coefficient ring
	  ring A
     ///,
     Caveat => {"The arrangements ", TT "MacLane", " and ", TT "Hessian", " are
	  defined over ", TT "ZZ/31627", ", where ", TT "6419", " is a cube root
	  of unity."}
}	  

document { 
     Key => (ring,Arrangement),
     Headline => "get the associated ring",
     Usage => "ring A",
     Inputs => {
	  "A" => Arrangement,
          },
     Outputs => { 
	  {"the ", TO2(Ring, "ring"), " that contains the defining
	  equations of ", TT "A"} 
	  },
     SeeAlso => {Arrangement,arrangement}
     }
document { 
     Key => (matrix,Arrangement),
     Headline => "create a matrix from the equations of an arrangement",
     Usage => "matrix A",
     Inputs => {
	  "A" => Arrangement,
          },
     Outputs => {
	  {"the ", TO2(Matrix, "matrix"), " constructed from the list
	  of defining equations of ", TT "A"}     	  
          },
     SeeAlso => {Arrangement,arrangement,coefficients}
     }
document { 
     Key => (coefficients,Arrangement),     
     Headline => "create a matrix from the coefficients of the
     equations of an arrangment",
     Usage => "coefficients A",
     Inputs => {
	  "A" => Arrangement
          },
     Outputs => {
	  Matrix
          }
     }
document { 
     Key => (rank,Arrangement),
     Headline => "compute the rank",
     Usage => "rank A",
     Inputs => {
	  "A" => Arrangement
          },
     Outputs => {
	  {"the rank of ", TT "A"}
          }
     }
document { 
     Key => (trim,Arrangement),
     Headline => "minimize the generators",
     Usage => "trim A",
     Inputs => {
	  "A" => Arrangement
          },
     Outputs => {
	  Arrangement
          }
     }
document { 
     Key => (compress,Arrangement),
     Headline => "extract nonzero equations",
     Usage => "compress A",
     Inputs => {
	  "A" => Arrangement
          },
     Outputs => {
	  Arrangement
          },
     Consequences => {
          }
     }

document { 
     Key => (dual,CentralArrangement),
     Headline => "the Gale dual of A",
     Usage => "dual A",
     Inputs => {
	  "A" => CentralArrangement
          },
     Outputs => {
	  CentralArrangement
          },
     "The Gale transform of a rank ", TT "l", "arrangement of ", TT "n",
     " hyperplanes is an arrangement of ", TT "n", " hyperplanes of rank ",
     TT "n-l", ".  Here it is computed as the arrangement given by the 
     rows of the matrix presenting the kernel of the cofficients of ", TT "A", "."
     }

document {
     Key => {typeA,(typeA,ZZ)},
     Headline => "Type A reflection arrangement",
     SeeAlso => {typeB, typeD},
     Usage => "typeA(n) or typeA(n,R) or typeA(n,k)",
     Inputs => {
	  "n" => ZZ => "the rank",
	  "R" => PolynomialRing => "a polynomial (coordinate) ring in n+1 variables",
	  "k" => Ring => {"a coefficient ring; by default, ", TT "QQ"}
	  },
     Outputs => {
	  Arrangement => "the A_n reflection arrangement"
	  },
     "The hyperplane arrangement with hyperplanes x_i-x_j.",
     EXAMPLE lines ///
          A3 = typeA(3)
	  describe A3
	  ring A3
	  ///,
     "Alternatively, one may specify a coordinate ring,",
     EXAMPLE lines ///
	  S = ZZ[w,x,y,z];
	  A3' = typeA(3,S)
	  describe A3'
	  ///,
     "or a coefficient ring:",
     EXAMPLE lines ///
     	  A4 = typeA(4,ZZ/3)
	  ring A4
	  ///
}

document {
     Key => (typeA,ZZ,PolynomialRing),
     Headline => "A_n arrangement with specified coordinate ring",
     SeeAlso => {typeB, typeD},
     Usage => "typeA(n,R)",
     Inputs => {
	  "n" => ZZ => "the rank",
	  "R" => PolynomialRing => "a polynomial (coordinate) ring in n+1 variables",
	  },
     Outputs => {
	  Arrangement => "the A_n reflection arrangement"
	  }
     }

document {
     Key => (typeA,ZZ,Ring),
     Headline => "A_n reflection arrangement with specified coefficient ring",
     SeeAlso => {typeB, typeD},
     Usage => "typeA(n,k)",
     Inputs => {
	  "n" => ZZ => "the rank",
	  "k" => Ring => "a coefficient ring"
	  },
     Outputs => {
	  Arrangement => {"the A_n reflection arrangement, over ", TT "k"}
	  }
     }

document {
     Key => {typeB,(typeB,ZZ),(typeB,ZZ,PolynomialRing),(typeB,ZZ,Ring)},
     Headline => "Type B reflection arrangement",
     SeeAlso => {typeA, typeD},
     Usage => "typeB(n) or typeB(n,R) or typeB(n,k)",
     Inputs => {
	  "n" => ZZ => "the rank",
	  "R" => PolynomialRing => "a polynomial (coordinate) ring in n variables",
	  "k" => Ring => {"a coefficient ring; by default, ", TT "QQ"}
	  },
     Outputs => {
	  Arrangement => "the B_n reflection arrangement"
	  },
     "The hyperplane arrangement with hyperplanes defined by the type-B_n root system: for example,",
     EXAMPLE {
          "B3 = typeB(3)",
	  "describe B3",
	  "ring B3",
 	  }
     }

document {
     Key => {typeD,(typeD,ZZ),(typeD,ZZ,PolynomialRing),(typeD,ZZ,Ring)},
     Headline => "Type D reflection arrangement",
     SeeAlso => {typeA, typeB},
     Usage => "typeD(n) or typeD(n,R) or typeD(n,k)",
     Inputs => {
	  "n" => ZZ => "the rank",
	  "R" => PolynomialRing => "a polynomial (coordinate) ring in n variables",
	  "k" => Ring => {"a coefficient ring; by default, ", TT "QQ"}
	  },
     Outputs => {
	  Arrangement => "the D_n reflection arrangement"
	  },
     "The hyperplane arrangement with hyperplanes defined by the type D_n root system: for example,",
     EXAMPLE {
          "D4 = typeD(4)",
	  "describe D4",
	  "ring D4",
	  }
     }

document { 
     Key => {randomArrangement, (randomArrangement,ZZ,ZZ,ZZ)},
     Headline => "generate an arrangement at random",
     Usage => "randomArrangement(n,l,N)",
     Inputs => {
	  "n" => ZZ => "number of hyperplanes",
	  "l" => ZZ => "dimension of ambient space",
	  "N" => ZZ => "absolute value of upper bound on coefficients"
          },
     Outputs => { 
	  Arrangement => {"a random, rational arrangement of ", TT "n", " hyperplanes
	       in ", TT "l", " variables."}
	  },
     "As ", TT "N", " increases, the random arrangement is a generic arrangement
     with probability tending to 1.",
     EXAMPLE lines ///
     	  randomArrangement(4,3,5)
	  tally apply(12, i -> poincare randomArrangement(6,3,5))
     ///
     }

document {
     Key => {orlikSolomon,(orlikSolomon,Arrangement),
	     (orlikSolomon,Arrangement,Ring),
	     (orlikSolomon,Arrangement,Symbol)},
     Headline => "defining ideal for the Orlik-Solomon algebra",
     Usage => "orlikSolomon(A) or orlikSolomon(A,E) or orlikSolomon(A,e)",
     Inputs => {
	  "A" => Arrangement,
	  "E" => Ring => "a skew-commutative polynomial ring
	   with one variable for each hyperplane",
	  "e" => Symbol => "a name for an indexed variable"
	  },
     Consequences => {
	  {"the list of ", TO circuits, " of ", TT "A", " is cached."}
          },
     Outputs => {
	  Ideal => {"the defining ideal of the Orlik-Solomon algebra of ", 
	            TT "A"}
	  },
     "The Orlik-Solomon algebra is the cohomology ring of the complement of 
      the hyperplanes, either in complex projective or affine space.  The
      optional Boolean argument ", TT "Projective", " specifies which.  The
      code for this method was written by Sorin Popescu.",
     EXAMPLE lines ///
     	  A = typeA(3)
     	  I = orlikSolomon(A,e)
	  reduceHilbert hilbertSeries I
	  I' = orlikSolomon(A,Projective=>true,HypAtInfinity=>2)
	  reduceHilbert hilbertSeries I'
     ///,
     PARA {}, "The code for ", TT "orlikSolomon", " was 
     contributed by Sorin Popescu."
     }
 
document {
     Key => [orlikSolomon,HypAtInfinity],
     Headline => "hyperplane at infinity",
     TT "HypAtInfinity => n", " specifies that n indexes the hyperplane 
     at infinity to be used in computing the (projective) Orlik-Solomon
     algebra.  Ignored without ", TT "Projective => true", "."
     }

document {
     Key => [orlikSolomon,Projective],
     Headline => "specify projective complement",
     TT "Projective => true", " specifies the cohomology ring of the
     complement of the hyperplanes in complex projective space.  For
     practical reasons (at the expense of some elegance) an affine chart
     for the complement is chosen by making the hyperplane numbered ", 
     TT "n", " the hyperplane at infinity.  By default, ", TT "n=0",
     "; otherwise, use the option ", TT "HypAtInfinity => n", "."
     }

document {
     Key => {orlikTerao,(orlikTerao,CentralArrangement),
	     (orlikTerao,CentralArrangement,PolynomialRing),
	     (orlikTerao,CentralArrangement,Symbol)},
     Headline => "defining ideal for the Orlik-Terao algebra",
     Usage => "orlikTerao(A) or orlikTerao(A,S) or orlikTerao(A,y)",
     Inputs => {
	  "A" => CentralArrangement => "a central hyperplane arrangement",
	  "S" => Ring => "a commutative polynomial ring
	   with one variable for each hyperplane",
	  "y" => Symbol => "a name for an indexed variable"
	  },
     Outputs => {
	  Ideal => {"the defining ideal of the Orlik-Terao algebra of ", 
	            TT "A"}
	  },
     "The Orlik-Terao algebra of an arrangement is the subalgebra of
      rational functions ", TT "k[1/f_1,1/f_2,...,1/f_n]", " where
      the ", TT "f_i", "'s are the defining forms for the hyperplanes.
      This method produces the kernel of the obvious surjection from a polynomial
      ring in ", TT "n", " variables onto the Orlik-Terao algebra.",
     EXAMPLE lines ///
     	  R := QQ[x,y,z];
	  orlikTerao arrangement {x,y,z,x+y+z}
     ///,
     " The defining ideal above has one generator given by the single relation ", 
     TT "x+y+z-(x+y+z)=0", ".  The rank-3 braid arrangement has four triple points: ",
     EXAMPLE lines ///
	  I := orlikTerao arrangement "braid"
	  betti res I
	  OT := comodule I;
	  apply(1+dim OT, i -> 0 == Ext^i(OT, ring OT))	  
     ///,
     " The Orlik-Terao algebra is always Cohen-Macaulay (Proudfoot-Speyer, 2006)."
     }

document {
     Key => Flat,
     Headline => "intersection of hyperplanes",
     PARA {},
     "A flat is a set of hyperplanes, maximal with respect to the property
     that they contain a given subspace.  In this package, flats are treated
     as lists of indices of hyperplanes in the arrangement."
     }

document {
     Key => {flat,(flat,Arrangement,List)},
     Headline => "make a flat from a list of indices",
     Usage => "flat(A,L)",
     Inputs => {
	  "A" => Arrangement => "hyperplane arrangement",
	  "L" => List => "list of indices in flat"
	  },
     Outputs => {
	  "F" => Flat => "corresponding flat"
	  }
     }

document {
     Key => {flats,(flats,ZZ,Arrangement),(flats,Arrangement)},
     Headline => "list the flats of an arrangement of given rank",
     SeeAlso => circuits,
     Usage => "flats(n,A)",
     Inputs => {
  	  "n" => ZZ => "rank",
	  "A" => Arrangement => "hyperplane arrangement"
	  },
     Outputs => {
	  "L" => List => {"A list of ", TO2{Flat,"flats"}, " of rank ", TT "n"}
	  },
     "If the rank is omitted, the ", TO2{Flat,"flats"}, " of each rank are
     listed.",
     EXAMPLE lines ///
     	  A := typeA(3)
	  flats(2,A)
     ///
     }
     
document {
     Key => {circuits,(circuits,Arrangement)},
     SeeAlso => flats,
     Headline => "list the circuits of an arrangement",
     Usage => "circuits(A)",
     Inputs => {
	  "A" => Arrangement => "hyperplane arrangement"
	  },
     Outputs => {
	  "L" => List => {"A list of circuits of ", TT "A", " each one expressed
	       as a list of indices."}
	  },
     "By definition, a circuit is a minimal set of hyperplanes with linearly 
     dependent normal vectors.",
     EXAMPLE lines ///
     	  R := QQ[x,y,z];
	  A := arrangement {x,y,z,x-y,x-z,y-z};
	  L := circuits A
	  (C -> (tolist A)_C)\L
     ///,
     "An arrangement has circuits of length 2 if and only if it has repeated 
     hyperplanes:",
     EXAMPLE lines ///
     	  A' := restriction(A,x)
	  circuits A'
     ///
}

document {
     Key => {closure,(closure,Arrangement,List)},
     Headline => "closure operation in the intersection lattice",
     Usage => "closure(A,L)",
     Inputs => {
	  "A" => Arrangement => "ambient arrangement",
	  "L" => List => "list of hyperplanes"
	  },
     Outputs => {
	  "F" => Flat => {
	       {"the flat of least codimension containing hyperplanes ", TT "L"}
		}
	  }
     }
     
document {
     Key => {meet,(meet,Flat,Flat)},
     Headline => "meet operation in intersection lattice",
     Usage => "meet(F,G)",
     Inputs => {
	  "F" => Flat => "a flat",
	  "G" => Flat => "another flat from the same arrangement"
	  },
     Outputs => {
	  "H" => Flat => {"the flat of greatest codimension that is
	  contained in both ", TT "F", " and ", TT "G", ".  If one identifies
	  flats with subspaces, this is the Minkowski sum of subspaces ", 
	  TT "F", " and ", TT "G", "."}
	  },
     "The operator ", TO (symbol ^, Flat, Flat), " can be used as a synonym."
     }

document {
     Key => {(symbol ^, Flat, Flat)},
     Headline => "meet operation in intersection lattice",
     SeeAlso => meet,
     "A synonym for ", TO (meet, Flat, Flat), "."
     }

document {
     Key => {vee,(vee,Flat,Flat)},
     Headline => "join operation in intersection lattice",
     Usage => "vee(F,G)",
     Inputs => {
	  "F" => Flat => "a flat",
	  "G" => Flat => "another flat from the same arrangement"
	  },
     Outputs => {
	  "H" => Flat => {"the flat of least codimension that
	  contains in both ", TT "F", " and ", TT "G", ".  If one identifies
	  flats with subspaces, this is the intersection of subspaces", 
	  TT "F", " and ", TT "G", "."}
	  },
     "The operator ", TO (symbol |, Flat, Flat), " can be used as a synonym."
     }

document {
     Key => {(symbol |, Flat, Flat)},
     Headline => "join operation in intersection lattice",
     SeeAlso => vee,
     "A synonym for ", TO (vee, Flat, Flat), "."
     }

document {
     Key => {(euler,Flat),(euler,Arrangement)},
     Usage => "euler(F) or euler(A)",
     Inputs => {
	  "F" => Flat => "a flat"
	  },
     Outputs => {
	  "k" => ZZ => {"the beta invariant of the flat ", TT "F", " or
	       arrangement ", TT "A"},
	       },
     "The beta invariant of an arrangement ", TT "A", " is, by definition, 
     the Euler characteristic of complement of ", TT "A", " in complex 
     projective space.", PARA {},
     "The beta invariant of a flat ", TT "F", " is the beta invariant of the ",
     TO2(restriction, "restriction"), " of ", TT "A", " to ", TT "F", ".",
     EXAMPLE lines ///
     	  A = typeA(3)
	  euler A -- for a real arrangement, equals number of bounded chambers
     ///
     }
	  
document {
     Key => {deletion,(deletion,Arrangement,RingElement)},
     Headline => "subarrangement given by deleting a hyperplane",
     Usage => "deletion(A,x)",
     Inputs => {
	  "A" => Arrangement => "a hyperplane arrangement",
	  "x" => RingElement => "equation of hyperplane to delete"
	    },
     Outputs => {
	  Arrangement => {"the subarrangement ", TT "A", " minus ", TT "x"}
	  },
     SeeAlso =>  (symbol ^,Arrangement,Flat),
     }
	  	  
document {
     Key => {restriction,(restriction,Arrangement,Flat),
	  (restriction,Flat),
  	  (restriction,Arrangement,RingElement),
	  (restriction,Arrangement,Ideal)},
     Headline => "restriction of arrangement to flat/hyperplane",
     Usage => "restriction(A,F) or restriction(A,x) or restriction(A,I)",
     Inputs => {
	  "A" => Arrangement => "a hyperplane arrangement (optional)",
	  "F" => Flat => "flat to which you restrict",
	  "x" => RingElement => "equation of hyperplane to which you restrict",
	  "I" => Ideal => "an ideal defining a subspace to which you restrict"
	    },
     Outputs => {
	  Arrangement => {"the restriction of ", TT "A"}
	  },
     "The restriction of an arrangement to the subspace ", TT "X", 
     " indexed by a flat is the (multi)set of hyperplanes ", TT "H intersect X",
     " for all ", TT "H", " in the arrangement ", TT "A", ".  In the first
     case, one can also write ", TO2((symbol ^, Arrangement, Flat), "A^F"), 
     ".",
     EXAMPLE lines ///
     	  A := typeA(3)
     	  L := flats(2,A)
	  A' := restriction first L
	  x := (ring A)_0  -- the subspace need not be in the arrangement
	  restriction(A,x)
     ///,
     "The restriction is, in general, a multiarrangement.  Use ", TO(trim),
     " to eliminate repeated hyperplanes.  For example,",
     EXAMPLE "trim A'",
     SeeAlso =>  (symbol ^,Arrangement,Flat),
     }

document {
     Key => (symbol ^, Arrangement, Flat),
     Headline => "restriction of arrangement to flat",
     Usage => "A^F",
     Inputs => {
	  "A" => Arrangement,
	  "F" => Flat => "the flat to which you restrict"
	  },
     Outputs => {
	  Arrangement => {"the restriction of ", TT "A", " to ", 
	  TT "F"}
     },
     "A synonym for ", TO(restriction), "."
     }

document {
     Key => {(cone,Arrangement,RingElement)},
-- can't write  {(cone,Arrangement,RingElement),(cone,Arrangement,Symbol)} ??
     Headline => "Cone of an arrangement",
     Usage => "cone(A,h) or cone(A,x)",
     Inputs => {
 	  "A" => Arrangement,
--	  "h" => Symbol,
	  "x" => RingElement => {"a variable in the ring of ", TT "A"}},
     Outputs => {
	  CentralArrangement => {"the cone over ", TT "A", "."}},
     "The cone of an affine arrangement is obtained from an arrangement
     by adding a linear hyperplane and homogenizing the remaining hyperplane
     equations with respect to it.",
     EXAMPLE lines ///
     	  R := QQ[x,y];
	  dA := arrangement {x,y,x-y,x-1,y-1}
	  A := cone(dA,symbol z)
     	  {dA,A} / isCentral
     ///,
          Caveat => {"If ", TT "x", " is a ring element, no checking is done to
	  verify it is a variable from outside the span of the hyperplanes of ",
	  TT "A","."},
     SeeAlso => (deCone,CentralArrangement,RingElement)
     }

document {
     Key => {deCone,(deCone,CentralArrangement,RingElement),(deCone,CentralArrangement,ZZ)},
     Headline => "produce an affine arrangement from a central one",
     Usage => "deCone(A,x) or deCone(A,i)",
     Inputs => {
 	  "A" => CentralArrangement,
	  "x" => RingElement => {"a hyperplane of ", TT "A"},
	  "i" => ZZ => {"the index of a hyperplane of ", TT "A"}},
     Outputs => { 
	  Arrangement => {"the dehomogenization of ", TT "A", " over ", TT "x"}},
     "The decone of a ", TO2(CentralArrangement, "central arrangement"), 
     " at a hyperplane ", TT "H=H_i", " or ", TT "H=ker x", " is 
     the affine arrangement obtained by choosing a chart in projective space 
     with ", TT "H", " as the hyperplane at infinity.",
     EXAMPLE lines ///
     	  A := arrangement "X3"
	  dA := deCone(A,2)
	  factor poincare A
	  poincare dA
     ///,
     SeeAlso => (cone,Arrangement,RingElement)
     }

document {
     Key => {subArrangement,(subArrangement,Arrangement,Flat),
	  (subArrangement,Flat)},
     Headline => "Subarrangement containing a fixed flat",
     Usage => "subArrangement(A,F) or subArrangement(F)",
     Inputs => {
	  "A" => Arrangement,
	  "F" => Flat
	  },
     Outputs => {
	  Arrangement => {"the subarrangement of ", TT "A", " contained in ",
	       TT "F"}
	  },
     "If ", TT "X", " is the linear subspace indexed by the flat ", TT "F",
     ", then the subarrangement ", TT "A_F", " consists of those hyperplanes 
     in ", TT "A", " that contain ", TT "X", ".",
     EXAMPLE lines ///
     	  A := typeA(3)
     	  flats(2,A)
	  B := subArrangement first oo
     ///,
     "Note that the ambient vector space of ", TT "A_F", " is the same as 
     that of ", TT "A", "; subarrangements are essential in general.",
     EXAMPLE lines ///
     	  ring B
     ///,     
     SeeAlso =>  (symbol _,Arrangement,Flat),
     }

document {
     Key => (symbol _, Arrangement, Flat),
     Headline => "Subarrangement containing a fixed flat",
     Usage => "A_F",
     Inputs => {
	  "A" => Arrangement,
	  "F" => Flat
	  },
     Outputs => {
	  Arrangement => {"the subarrangement of ", TT "A", " containing ", 
	  TT "F"}
     },
     "A synonym for ", TO(subArrangement), "."
     }

document {
     Key => {graphic, (graphic,List), (graphic,List,Ring), 
	  (graphic,List,PolynomialRing)},
     Headline => "Make a graphic arrangement",
     Usage => "graphic(G) or graphic(G,k) or graphic(G,R)",
     Inputs => {
	  "G" => List => "a graph, expressed as a list of pairs of vertices",
	  "k" => Ring => {"an optional coefficient ring, by default ", 
	       TT "QQ"},
	  "R" => PolynomialRing => "an optional coordinate ring for the 
	  arrangement"
     },
     Outputs => {
	  Arrangement => {"the graphic arrangement from graph ", TT "G"}
     },
     SeeAlso => typeA,
     "A graph ", TT "G", " has vertices 1, 2, ..., n, and its edges are
     a list of lists of length 2.  The graphic arrangement ", TT "A(G)", 
     " of ", TT "G", " is, by definition, the subarrangement of the
     type A_(n-1) arrangement with hyperplanes ", TT "x_i-x_j", " for
     each edge ", TT "{i,j}", " of ", TT "G",
     EXAMPLE lines ///
     	  G = {{1,2},{2,3},{3,4},{4,1}}; -- a four-cycle
	  AG = graphic G
	  describe AG
	  rank AG -- the number of vertices minus number of components
	  ring AG
	  ring graphic(G,ZZ[x,y,z,w])
	  ///
	  }
     
document {
     Key => {der, (der,CentralArrangement), (der,CentralArrangement,List)},
     Headline => "Module of logarithmic derivations",
     Usage => "der(A) or der(A,m)",
     Inputs => {
	  "A" => CentralArrangement => "a central arrangement, possibly with repeated hyperplanes",
	  "m" => List => "optional list of multiplicities, one for each hyperplane"
     },
     Outputs => {
	  Matrix => {"A matrix whose image is the module of logarithmic derivations corresponding
	              to the (multi)arrangement ", TT "A", "; see below."}
		      },
     "The module of logarithmic derivations of an arrangement defined
     over a ring ", TT "S", " is, by definition, the submodule of ", TT "S", 
     "-derivations with the property that ", TT "D(f_i)", " is contained
     in the ideal generated by ", TT "f_i", " for each linear form ", 
     TT "f_i", " in the arrangement.", PARA {},
     "More generally, if the linear form ", TT "f_i", " is given a 
     positive integer multiplicity ", TT "m_i", ", then the logarithmic
     derivations are those ", TT "D", " with the property that ", 
     TT "D(f_i)", " is in ", TT "ideal(f_i^(m_i))", 
     " for each linear form ", TT "f_i", ".",
     PARA {},
     "The ", TT "j", "th column of the output matrix expresses the ", TT "j", "th generator
     of the derivation module in terms of its value on each linear form, in order.",
     EXAMPLE {
	  "R = QQ[x,y,z];",
	  "der arrangement {x,y,z,x-y,x-z,y-z}"
     }, PARA {},     
     "This method is implemented in such a way that any derivations of 
     degree 0 are ignored.  Equivalently, the arrangement ", TT "A", " is
     forced to be essential: that is, the intersection of all the hyperplanes
     is the origin.",
     EXAMPLE lines ///
     	  prune image der typeA(3)
	  prune image der typeB(4) -- A is said to be free if der(A) is a free module
	  ///,
     "not all arrangements are free:",
     EXAMPLE lines ///
	  R = QQ[x,y,z];
	  A = arrangement {x,y,z,x+y+z}
	  betti res prune image der A
	  ///,
     "If a list of multiplicities is not provided, the occurrences of 
     each hyperplane are counted:",
     EXAMPLE lines ///
     	  R = QQ[x,y]
	  prune image der arrangement {x,y,x-y,y-x,y,2*x}   -- rank 2 => free
	  prune image der(arrangement {x,y,x-y}, {2,2,2})  -- same thing
	  ///
     }	  

document {
     Key => [der,Strategy],
     "If an arrangement has (squarefree) defining polynomial ", TT "Q", ", 
     then the logarithmic derivations are those ", TT "f", " for which ",
     TT "f(Q)", " is in the ideal ", TT "(Q)", ".  The Classic strategy
     assumes that the arrangement is simple and implements this definition.
     By contrast, the default strategy treats all arrangements as
     multiarrangements."
     }     

document {
     Key => {multIdeal, (multIdeal,QQ,CentralArrangement), 
	  (multIdeal,QQ,CentralArrangement,List),
	  (multIdeal,ZZ,CentralArrangement),
	  (multIdeal,ZZ,CentralArrangement,List)},
     Headline => "compute a multiplier ideal",
     Usage => "multIdeal(s,A) or multIdeal(s,A,m)",
     Inputs => {
	  "A" => CentralArrangement => "a central hyperplane arrangement",
	  "s" => QQ => "a rational number",
	  "m" => List => "optional list of positive integer multiplicities",
     },
     Outputs => {
	  Ideal => {"the multiplier ideal of the arrangement at the value",
	       TT "s"},
     },
     "The multiplier ideals of an given ideal depend on a nonnegative
     real parameter.  This method computes the multiplier ideals of
     the defining ideal of a hyperplane arrangement, optionally with
     multiplicities ", TT "m", ".  This uses the
     explicit formula of M. Mustata [TAMS 358 (2006), no 11, 5015--5023], as 
     simplified by Z. Teitler [PAMS 136 (2008), no 5, 1902--1913].",
     PARA {}, "Let's consider Example 6.3 of Berkesch and Leykin from 
     arXiv:1002.1475v2:",
     EXAMPLE lines ///
          R := QQ[x,y,z];
	  A := arrangement ((x^2 - y^2)*(x^2 - z^2)*(y^2 - z^2)*z);
	  multIdeal(3/7,A)
     ///,
     "Since the multiplier ideal is a step function of its
     real parameter, one tests to see at what values it changes:",
     EXAMPLE {
	  "H = new MutableHashTable",
	  "scan(39,i -> (
	  	    s := i/21;
	  	    I := multIdeal(s,A);
	  	    if not H#?I then H#I = {s} else H#I = H#I|{s}));",
	  "netList sort values H -- values of s giving same multiplier ideal"
	  },
     SeeAlso => lct
}

document {
     Key => {lct, (lct,CentralArrangement)},
     Headline => "Compute the log-canonical threshold of an arrangement",
     Usage => "lct(A)",
     Inputs => {
	  "A" => CentralArrangement => "a central hyperplane arrangement"},
     Outputs => {
	  QQ => {"The log-canonical threshold of ", TT "A", "."}},
     "The log-canonical threshold of ", TT "A", " defined by a polynomial ",
     TT "f", " is the least number ", TT "c", " for which the multiplier ideal ",
     TT "J(f^c)", " is nontrivial.",
     PARA {}, "Let's consider Example 6.3 of Berkesch and Leykin from 
     arXiv:1002.1475v2:",
     EXAMPLE lines ///
          R := QQ[x,y,z];
	  A := arrangement ((x^2 - y^2)*(x^2 - z^2)*(y^2 - z^2)*z);
	  lct A
     ///,
     "note that ", TT "A", " is allowed to be a multiarrangement.",
     SeeAlso => multIdeal
}

document {
     Key => {EPY, (EPY,Arrangement), (EPY,Ideal), 
	  (EPY,Arrangement,PolynomialRing),(EPY,Ideal,PolynomialRing)},
     Headline => "compute the Eisenbud-Popescu-Yuzvinsky module of an arrangement",
     Usage => "EPY(A) or EPY(A,S) or EPY(I) or EPY(I,S)",
     Inputs => {
	  "A" => Arrangement => "an arrangement of n hyperplanes",
	  "I" => Ideal => "an ideal of the exterior algebra, the quotient by which has a 
	  linear, injective resolution",
	  "S" => PolynomialRing => "an optional polynomial ring in n variables",
     },
     Outputs => {
	  Module => {"The Eisenbud-Popescu-Yuzvinsky module (see below) of ", TT "I", 
	       " or, if an arrangement is given, of its Orlik-Solomon ideal."}
     },
     "Let ", TT "OS", " denote the ", TO2(orlikSolomon, "Orlik-Solomon algebra"), " of 
     the arrangement ", TT "A", 
     ", regarded as a quotient of an exterior algebra ", TT "E", ".  The module ", 
     TT "EPY(A)", " is, by definition, the ", TT "S", "-module which is BGG-dual to
     the linear, injective resolution of ", TT "OS", " as an ", TT "E", "-module.",
     PARA {},
     "Equivalently, ", TT "EPY(A)", " is the single nonzero cohomology module in the
     Aomoto complex of ", TT "A", ".  For details, see Eisenbud-Popescu-Yuzvinsky, 
     [TAMS 355 (2003), no 11, 4365--4383].",
     EXAMPLE lines ///
     	  R = QQ[x,y];
	  FA = EPY arrangement {x,y,x-y}
	  betti res FA
     ///,
     "In particular, ", TT "EPY(A)", " has a linear free resolution over the polynomial ring,
     namely the Aomoto complex of ", TT "A", ".",
     EXAMPLE { -- change to typeB(3) if this takes too long
	  "A = typeA(4)",
	  "factor poincare A",
	  "betti res EPY A"
     }
}

document {
     Key => {isDecomposable, (isDecomposable,CentralArrangement), 
	     (isDecomposable,CentralArrangement,Ring)},
     Headline => "test if an arrangement is decomposable",
     Usage => "isDecomposable(A) or isDecomposable(A,k)",
     Inputs => {
	  "A" => CentralArrangement => "a hyperplane arrangement",
	  "k" => Ring => "an optional coefficient ring, by default the
	       coefficient ring of the arrangement",
     },
     Outputs => {
	  Boolean => {"whether or not the arrangement decomposes in the
	       sense of Papadima and Suciu [Comment. Helv. 2006]"}
     },
     "An arrangement is said to be decomposable if the derived subalgebra of
     its holonomy Lie algebra is a direct sum of the derived subalgebras of
     free Lie algebras, indexed by the rank-2 ", TO2{Flat,"flats"}, 
     " of the arrangement.  At the time of writing, it is not known if
     the answer depends on the characteristic of the coefficient ring.",
     EXAMPLE lines ///
	  X3 = arrangement "X3"
	  isDecomposable X3
	  isDecomposable(X3,ZZ/5)
	  isDecomposable typeA(3)
	  ///
}

TEST ///
R = ZZ[x,y,z];
trivial = arrangement({},R);
nontrivial = arrangement({x},R);
assert(rank trivial == 0)
assert(ring trivial === R)
assert(0 == matrix trivial)
assert(0 == coefficients trivial)
assert(deletion(nontrivial,x) == trivial)
assert(trivial**trivial != trivial)
assert(trivial**QQ != trivial)
trim trivial

A = typeA(3)
assert((prune image der A) == (ring A)^{-1,-2,-3})  -- free module of derivations?
assert((prune image der(A, {2,2,2,2,2,2})) == (ring A)^{-4,-4,-4})

A3 = arrangement({x,y,z,x-y,x-z,y-z},R)
describe A3
assert(rank A3 == 3)
assert(pdim EPY (A3**QQ) == 3)
assert(not isDecomposable A3)

X3 = arrangement "X3"
assert(isDecomposable X3)
assert(multIdeal(2,X3) == multIdeal(11/5,X3))
time I1 = orlikTerao(X3);
time I2 = orlikTerao(X3,ring I1,NaiveAlgorithm=>true);
assert(I1==I2)

M = arrangement "MacLane"
P = poincare M
t = (ring P)_0
assert(1+8*t+20*t^2+13*t^3 == P)

///
end

--A3' = arrangement {x,y,z,x-y,x-z,y-z}
--A3' == A3
--product A3
--A3.hyperplanes

--NF = arrangement {x,y,z,x-y,x-z,y-z,x+y-z}
--///

end

document {
     Key => HyperplaneArrangements,
     Headline => "hyperplane arrangements",
     EM "HyperplaneArrangements", " is a package for manipulating hyperplane
     arrangements.",
     "A hyperplane arrangement is a finite set of hyperplanes in an
     affine or projective space.  In this package, an arrangement is 
     expressed as a list of (linear) defining equations for the hyperplanes.
     The tools provided allow the user to create new arrangements from
     old, and to compute various algebraic invariants of arrangements."
}

document {
     Key => Arrangement,
     Headline => "the class of all arrangements",
     TT "Arrangement", " -- the class of hyperplane arrangements"
}

document (
     Key => arrangement,
     Headline => "create a hyperplane arrangement",
     Usage => "arrangement L",
     Inputs => (
	  "L" => List => "a list of ring elements defining the hyperplanes"
	  ),
     Outputs => (
	  Arrangement => ("the arrangement with hyperplanes ", TT "L")
	  ),
     "A hyperplane arrangement consists of a list of degree-1 ring elements."
     )
     
end

path = append(path, homeDirectory | "exp/hyppack/")
installPackage("HyperplaneArrangements",RemakeAllDocumentation=>true,DebuggingMode => true)
loadPackage "HyperplaneArrangements"
