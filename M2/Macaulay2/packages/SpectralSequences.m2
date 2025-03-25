-- -*- coding: utf-8 -*-
--------------------------------------------------------------------------------
-- Copyright 2012  Gregory G. Smith
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
  "SpectralSequences",
--  AuxiliaryFiles => true,
  Version => "1.0",
  Date => "20 September 2016",
  Authors => {
       {
      Name => "David Berlekamp", 
      Email => "daffyd@math.berkeley.edu", 
      HomePage => "http://www.math.berkeley.edu/~daffyd"},
    {
      Name => "Adam Boocher", 
      Email => "boocher@math.utah.edu", 
      HomePage => "http://www.math.utah.edu/~boocher"},
       {
      Name => "Nathan Grieve", 
      Email => "n.grieve@unb.ca",
      HomePage => "http://www.math.unb.ca/~ngrieve"},  
  {
      Name => "Eloisa Grifo", 
      Email => "eloisa.grifo@virginia.edu",
      HomePage => "http://people.virginia.edu/~er2eq/"},             
    {
      Name => "Gregory G. Smith", 
      Email => "ggsmith@mast.queensu.ca", 
      HomePage => "http://www.mast.queensu.ca/~ggsmith"},
 {
      Name => "Thanh Vu", 
      Email => "vqthanh@math.berkeley.edu",
      HomePage => "http://math.berkeley.edu/~thanh"}},
  Headline => "spectral sequences",
  Keywords => {"Homological Algebra"},
  PackageImports => {"Truncations"},
  PackageExports => {"SimplicialComplexes", "ChainComplexExtras", "PushForward"}
  )

export {
  "FilteredComplex",
  "filteredComplex",
  "SpectralSequence",
  "spectralSequence", 
  "spots",
  "SpectralSequencePage", 
  "spectralSequencePage",
  "homologyIsomorphism",
  "Shift",
  "ReducedHomology",
  "SpectralSequencePageMap",
 "spectralSequencePageMap",
  "connectingMorphism",
  "sourcePruningMap",
  "targetPruningMap",
   "Page", 
   "PageMap", 
   "pageMap", 
   "page" ,
  "pruningMaps", "edgeComplex",
  "filteredHomologyObject", "associatedGradedHomologyObject", "netPage"
  }


protect inducedMaps

--------------------------------------------------------------------------------

hasAttribute = value Core#"private dictionary"#"hasAttribute"
getAttribute = value Core#"private dictionary"#"getAttribute"
ReverseDictionary = value Core#"private dictionary"#"ReverseDictionary"

------------------------------------------------------
------------------------------------------------------


--------------------------------------------------------------------------------
-- CODE
--------------------------------------------------------------------------------
------------------------------------------------------------------------------------
-- ChainComplexExtraExtras -- Several people have worked on this portion of the code
--------------------------------------------------------------------------------------

-- since things are mutable we don't want to cache spots
spots = method()

spots ChainComplex := List => (
  C -> sort select(keys complete C,i -> class i === ZZ))

max ChainComplex := K -> max spots K
min ChainComplex := K -> min spots K

support ChainComplex := List => (
     C -> sort select (spots C, i -> C_i != 0))


-- Computes the graded pieces of the total complex of a Hom double complex 
-- (just as a graded module, so no maps!)
Hom (GradedModule, GradedModule) := GradedModule => opts -> (C, D) -> (
  R := C.ring;  if R =!= D.ring then error "expected graded modules over the same ring";
  (c,d) := (spots C, spots D);
  pairs := new MutableHashTable;
  scan(c, i -> scan(d, j -> (
        k := j-i;
	p := if not pairs#?k then pairs#k = new MutableHashTable else pairs#k;
	p#(i,j) = 1;)));
  scan(keys pairs, k -> pairs#k = sort keys pairs#k);
  E := new GradedModule;
  E.ring = R;
  scan(keys pairs, k-> (
      p := pairs#k;
      E#k = directSum(apply(p, v -> v => Hom(C_(v#0), D_(v#1), opts)));));
  E)



isWellDefined ChainComplexMap := Boolean => f -> (
     (F,G):= (source f, target f);
     all(drop(spots F,1), i -> G.dd_i * f#i == f#(i-1) * F.dd_i))

-- Computes the total complex of the Hom double complex of two chain complexes
-- This code is different from that in ChainComplexExtras.  We need this version
-- so that the indices are cached.
Hom (ChainComplex, ChainComplex) := ChainComplex => opts -> (C, D) -> (
  if C.ring =!= D.ring then error "expected chain complexes over the same ring";
  hom := lookup(Hom, GradedModule, GradedModule);
  E := chainComplex (hom opts)(C, D);
  scan(spots E, i -> if E#?i and E#?(i-1) then E.dd#i = 
    map(E#(i-1), E#i, 
      matrix table(
        E#(i-1).cache.indices, E#i.cache.indices, 
	(j,k) -> map(E#(i-1).cache.components#(E#(i-1).cache.indexComponents#j), 
	  (E#i).cache.components#((E#i).cache.indexComponents#k),
	  if j#0 === k#0 and j#1 === k#1-1 then (-1)^(k#0)*Hom(C_(k#0), D.dd_(k#1), opts)
	  else if j#0 === k#0 + 1 and j#1 === k#1 then Hom(C.dd_(j#0), D_(k#1), opts)
	  else 0))));
  E    	    		    
)

Hom (ChainComplex, ChainComplexMap) := ChainComplexMap => opts -> (C, f) -> (
  (F, G) := (Hom(C, source f, opts), Hom(C, target f, opts));
  map(G,F, i -> map(G_i,F_i, matrix table( G_i.cache.indices,F_i.cache.indices, 
      (j,k) -> map(G#i.cache.components#(G#i.cache.indexComponents#j), 
        F#i.cache.components#(F#i.cache.indexComponents#k),
	if j === k then Hom(C_(j#0), f_(j#1), opts)
	else 0)))))

Hom (ChainComplexMap, ChainComplex) := ChainComplexMap => opts -> (f, C) -> (
  (F, G) := (Hom(target f, C, opts), Hom(source f, C, opts));
  map(G,F, i -> map (G_i,F_i, matrix table(G_i.cache.indices,F_i.cache.indices,
        (j,k) -> map(G#i.cache.components#(G#i.cache.indexComponents#j), 
	  F#i.cache.components#(F#i.cache.indexComponents#k),
	  if j === k then Hom(f_(j#0), C_(j#1), opts)
	  else 0)))))
  
ChainComplexMap ** ChainComplex := ChainComplexMap => (f,C) -> (
  (F,G) := ((source f) ** C, (target f) ** C); 
  map(G,F, i -> map (G_i,F_i, matrix table(G_i.cache.indices,F_i.cache.indices,
        (j,k) -> map(G#i.cache.components#(G#i.cache.indexComponents#j), 
	  F#i.cache.components#(F#i.cache.indexComponents#k),
	  if j === k then f_(j#0) ** C_(j#1) 
	  else 0)))))

ChainComplex ** ChainComplexMap := ChainComplexMap => (C,f) -> (
  (F,G) := (C ** source f, C ** target f); 
  map(G,F, i -> map (G_i,F_i, matrix table(G_i.cache.indices,F_i.cache.indices,
        (j,k) -> map(G#i.cache.components#(G#i.cache.indexComponents#j), 
	  F#i.cache.components#(F#i.cache.indexComponents#k),
	  if j === k then C_(j#0) ** f_(j#1) 
	  else 0)))))

-- truncate a chain complex at a given homological degree 
truncate(ChainComplex,ZZ):= {} >> o -> (C,q) ->(
     if q == 0 then return C 
     else (
	  m := min support C;
	  n := max support C;
	  l := length C;
	  if q < -l or q > l then return image(0*id_C)
	  else  K:=new ChainComplex;
	        K.ring=C.ring;
	  	if q < 0 then for i from min C + 1 to max C do (
	             if i <= n + q then K.dd_i = C.dd_i 
	       	     else if i-1 > n + q then K.dd_i = inducedMap(0*C_(i-1),0*C_i,C.dd_i)
	       	     else K.dd_i = inducedMap(C_(i-1), 0*C_i, C.dd_i) ) 
	  	else for i from min C+1  to max C do (
	       	     if i-1 >= q + m then K.dd_i = C.dd_i 
	       	     else if i < q + m then K.dd_i = inducedMap(0*C_(i-1),0*C_i,C.dd_i)
	       	     else K.dd_i = map(0*C_(i-1), C_i, 0*C.dd_i) )); 		
     K)


-- the following relies on the pushFwd method from the package "PushForward.m2"

pushFwd(RingMap,ChainComplex):=o->(f,C) ->
(    pushFwdC := chainComplex(source f);
     maps := apply(spots C, i-> (i,pushFwd(f,C.dd_i)));
     for i from min C to max C do (
	 pushFwdC.dd_(maps#i_0) = maps#i_1 
	 );
    pushFwdC
    )


-- New method for tensor that returns the tensor product of a complex via a ring map
tensor(RingMap, ChainComplex) := ChainComplex => {} >> opts -> (f,C) -> (
         k := min C; 
    D := chainComplex(
	if even(k) then apply(
	    drop(select(keys complete C, 
	    	i -> instance(i,ZZ)),1), 
	    j -> f ** C.dd_j)
	else apply(
	    drop(select(keys complete C, 
	    	i -> instance(i,ZZ)),1), 
	    j -> (-1) * (f ** C.dd_j)));
    D[-k]
    )


----------------------------------------------------------------------------------

-------------------------------------------------------------------------------------
-- filtered complexes
-------------------------------------------------------------------------------------
FilteredComplex = new Type of HashTable
FilteredComplex.synonym = "filtered chain complex"

spots FilteredComplex := List => (
  K -> sort select(keys K, i -> class i === ZZ))

max FilteredComplex := K -> max spots K
min FilteredComplex := K -> min spots K

support FilteredComplex := List => (
     K -> sort select (spots K, i -> K#i != 0))


FilteredComplex _ InfiniteNumber :=
FilteredComplex _ ZZ := ChainComplex => (K,p) -> (
  if K#?p then K#p 
  else if p < min K then K#(min K) 
  else if p > max K then K#(max K)
  )

FilteredComplex ^ InfiniteNumber :=
FilteredComplex ^ ZZ := ChainComplex => (K,p) -> K_(-p)

chainComplex FilteredComplex := ChainComplex => K -> K_infinity

-- Returns the inclusion map from the pth subcomplex to the top
inducedMap (FilteredComplex, ZZ) := ChainComplexMap => opts -> (K,p) -> (
  if not K.cache#?inducedMaps then K.cache.inducedMaps = new MutableHashTable;
  if not K.cache.inducedMaps#?p then K.cache.inducedMaps#p = inducedMap(K_infinity, K_p);
  K.cache.inducedMaps#p)


net FilteredComplex := K -> (
  v := between("", apply(spots K, p -> p | " : " | net K_p));
  if #v === 0 then "0" else stack v)


-- Primitive constructor, takes a list eg {m_n,m_(n-1), ...,m_0} 
-- defining inclusion maps C=F_(n+1)C > F_(n)C > ... > F_0 C 
-- of subcomplexes of a chain complex (or simplicial complexes) 
-- and produces a filtered complex with integer keys the
-- corresponding chain complex.
-- If F_0C is not zero then by default F_(-1)C is added and is 0.
-- THIS IS THE CONVENTION WE WANT BY DEFAULT.  SEE 
-- THE HOPF FIBRATION EXAMPLE.  TO GET THE CORRECT INDICES ON THE E2 PAGE
-- WE WANT THE ZERO COMPLEX TO HAVE "FILTRATION DEGREE -1".

filteredComplex = method(Options => {
    Shift => 0,
    ReducedHomology => true})

filteredComplex(List) := FilteredComplex => opts -> L -> (
  local maps;
  local C;
  if #L === 0 
  then error "expected at least one chain complex map or simplicial complex";
  if all(#L, p -> class L#p === SimplicialComplex) then (
    kk := coefficientRing L#0;
    if opts.ReducedHomology == true then (
    C = chainComplex complex L#0; -- By default the ambient simplicial complex is the first element of the list
    maps = apply(#L-1, p -> map(C, chainComplex complex L#(p+1), 
        i -> sub(contract(transpose matrix{faces(i,L#0)}, matrix{faces(i,L#(p+1))}), kk))))
    else (C = truncate(chainComplex complex L#0,1); -- By default the ambient simplicial complex is the first element of the list
    maps = apply(#L-1, p -> map(C, truncate(chainComplex complex L#(p+1),1), 
        i -> sub(contract(transpose matrix{faces(i,L#0)}, matrix{faces(i,L#(p+1))}), kk))))   
 )
  else (
    maps = L;
    if any(#maps, p -> class maps#p =!= ChainComplexMap) then (
      error "expected sequence of chain complexes");
    C = target maps#0;-- By default the ambient chain complex is target of first map.
    if any(#maps, p -> target maps#p != C) then (
      error "expected all map to have the same target"));     
  Z := image map(C, C, i -> 0*id_(C#i)); -- make zero subcomplex as a subcomplex of ambient complex 
   P := {};
 myList := {};
 for p from 0 to #maps - 1 do (
	 myList = myList |
	  {#maps - (p+1) -opts.Shift => image maps#p};
	  );
  if myList != {} then (P = {(#maps-opts.Shift) => C} | myList)
  else P = { - opts.Shift => C} ;
  if (last P)#1 != Z then (P = P | {(-1-opts.Shift) => Z});
  return new FilteredComplex from P | {symbol zero => (ring C)^0, symbol cache =>  new CacheTable})


--------------------------------------------------------------------------------
-- constructing filtered complexes ---------------------------------------------
--------------------------------------------------------------------------------


-- make the filtered complex associated to the "naive truncation of a chain complex"
filteredComplex ChainComplex := FilteredComplex => opts-> C->( complete C; 
    n := max support C;
    m := min support C;
    p := length C;
    if p > 0  then (
    H := for i from 1 to p list inducedMap(C,truncate(C,-i));
    filteredComplex( H, Shift => - m) )
    else filteredComplex {map(C, image(0 * id_C), id_C)}--{map(C, id_C} -- now the constructor supports the zero chain complex
	      )


--produce the "x-filtration" of the tensor product complex.
FilteredComplex ** ChainComplex := FilteredComplex => (K,C) -> ( 
     xTensormodules := (p,q,T)->(apply( (T#q).cache.indices,
     i-> if (i#0) <=p then  
     image (id_(((T#q).cache.components)#(((T#q).cache.indexComponents)#i)))
     else image(0* id_(((T#q).cache.components)#(((T#q).cache.indexComponents)#i)))) );
     xTensorComplex := (T,p) ->(K := new ChainComplex;
		    K.ring = T.ring;
		    for i from min T to max T do (
		    if T#?(i-1) then
		    K.dd_i = inducedMap(
			 directSum(xTensormodules(p,i-1,T)
			      ),
			 directSum(xTensormodules(p,i,T)),T.dd_i));
       	       K
		    );
		     supp := support K_infinity;
     -- try to handle the boundary cases --
     if supp != {} and #supp > 1 then (		
     	  N := max support K_infinity;
	  P := min support K_infinity;
	  T := K_infinity ** C;
filteredComplex(reverse for i from P to (N-1) list 
     inducedMap(T, xTensorComplex(T,i)), Shift => -P) 
 )
    else ( if #supp == 1 then
	(
	p := min supp;
	t := K_infinity ** C;
	filteredComplex( {inducedMap(t, xTensorComplex(t, p))}, Shift => - p + 1)
	)
	else( tt:= K_infinity ** C;
	    filteredComplex({id_tt})
	    )
	)
     )

--produce the "y-filtration" of the tensor product complex.
ChainComplex ** FilteredComplex := FilteredComplex => (C,K) -> ( 
     yTensorModules := (p,q,T)->(apply( (T#q).cache.indices,
     i-> if (i#1) <=p then  image (id_(((T#q).cache.components)#(((T#q).cache.indexComponents)#i)))
     else image(0* id_(((T#q).cache.components)#(((T#q).cache.indexComponents)#i)))) );
    yTensorComplex := (T,p) -> (K := new ChainComplex;
		    K.ring = T.ring;
		    for i from min T to max T do (
		    if T#?(i-1) then
	     	    K.dd_i = inducedMap(directSum(yTensorModules(p,i-1,T)),
			 directSum(yTensorModules(p,i,T)),T.dd_i));
	       K
	       );
	   supp := support K_infinity;
	        -- try to handle the boundary cases --
     if supp != {} and #supp > 1 then (		
     	  N := max support K_infinity;
	  P := min support K_infinity;
	  T := C ** K_infinity;
filteredComplex(reverse for i from P to (N-1) list 
     inducedMap(T, yTensorComplex(T,i)), Shift => -P) 
 )
    else ( if #supp == 1 then
	(
	p := min supp;
	t := C ** K_infinity;
	filteredComplex( {inducedMap(t, yTensorComplex(t, p))}, Shift => - p + 1)
	)
	else( tt:= C ** K_infinity ;
	    filteredComplex({id_tt})
	    )
	)
     )	   

-- produce the "x-filtration" of the Hom complex.
xmodules := (n, d, H)->(
    -- want components {p,q} = Hom(-p, q) with p + q = d and p <= n
     apply( (H#d).cache.indices,
     i -> if  - (i#0) <= n then  
     image (id_(((H#d).cache.components)#(((H#d).cache.indexComponents)#i)))
     else image(0* id_(((H#d).cache.components)#(((H#d).cache.indexComponents)#i)))) );


xComplex := (T,n) -> 
     	       (K := new ChainComplex;
		    K.ring = T.ring;
		    for i from min T to max T do (
		    if T#?(i-1) then
		    K.dd_i = inducedMap(directSum(xmodules(n,i-1,T)),directSum(xmodules(n,i,T)),T.dd_i));
	       K
	       )

-- produce the "x-filtration" of the Hom complex.
Hom (FilteredComplex, ChainComplex):= FilteredComplex => opts -> (K, D) -> (
    	C := complete D;
    	   supp := support K_infinity;
	        -- try to handle the boundary cases --
     if supp != {} and #supp > 1 then (		
     N := - max support K_infinity;
     P := - min support K_infinity;
     H := Hom(K_infinity, C, opts);
     filteredComplex(reverse for i from N to P - 1 list inducedMap(H, xComplex(H,i)), 
	 Shift => - N)
     )
 else ( if #supp == 1 then
	(
	p := min supp;
	h := Hom(K_infinity, C, opts);
	filteredComplex( {inducedMap(h, xComplex(h, p))}, Shift =>  p + 1 )
	)
	else(
	    hhh := Hom(K_infinity, C, opts);
	    filteredComplex({id_hhh})
	    )
	)
    )

-- next are some functions used in the "y-filtration" of the Hom complex.

ymodules := (n, d, H) -> (
    -- want components {p,q} = Hom(-p, q) with p + q = d and q <= n
     apply( (H#d).cache.indices,
     i -> if   (i#1) <= n then  
     image (id_(((H#d).cache.components)#(((H#d).cache.indexComponents)#i)))
     else image(0* id_(((H#d).cache.components)#(((H#d).cache.indexComponents)#i)))) 
 )


yComplex := (T,n) -> 
     	       (K := new ChainComplex;
		    K.ring = T.ring;
		    for i from min T to max T do (
		    if T#?(i-1) then
		    K.dd_i = inducedMap(directSum(ymodules(n,i-1,T)),directSum(ymodules(n,i,T)),T.dd_i));
	       K
	       )

Hom (ChainComplex, FilteredComplex) := FilteredComplex => opts -> (D, K) -> (
      C := complete D; 
     supp := support K_infinity;
	        -- try to handle the boundary cases --
     if supp != {} and #supp > 1 then (		
     N :=  max support K_infinity;
     P :=  min support K_infinity;
     H := Hom(C, K_infinity, opts);
     filteredComplex(reverse for i from P to N - 1 list inducedMap(H, yComplex(H,i)), 
	 Shift => - P)
     )
  else ( if #supp == 1 then
	(
	p := min supp;
	h := Hom(C, K_infinity, opts);
	filteredComplex( {inducedMap(h, yComplex(h, p))}, Shift =>  - p  + 1 )
	)
	else(
	    hhh := Hom(C, K_infinity, opts);
	    filteredComplex({id_hhh})
	    )
	) 
    )


-- I-adic filtration code --
-- the following script allows us to multiply a chain complex by an ideal
Ideal * ChainComplex := ChainComplex => (I,C) -> (
    D := new ChainComplex;
    D.ring = C.ring;
    apply(drop(spots C, 1), i -> D.dd_i = inducedMap(I * C_(i-1), I * C_i, C.dd_i));
    D
    )

filteredComplex(Ideal,ChainComplex,ZZ) := FilteredComplex => opts -> (I,C,n) ->(
    if n < 0 then error "expected a non-negative integer"
    else
    filteredComplex(apply(n, i -> inducedMap(C, I^(i+1) * C)), Shift => n)   
    )

------------------------------------
-- Pages and Sequences --
------------------------------------


--------------------------------------------------------------------------------
-- Pages
--------------------------------------------------------------------------------
Page = new Type of MutableHashTable
Page.synonym = "Page"
Page.GlobalAssignHook = globalAssignFunction
Page.GlobalReleaseHook = globalReleaseFunction
describe Page := E -> net expression E

new Page := Page => (cl) -> (
     C := newClass(Page,new MutableHashTable); -- sigh
     C.cache = new CacheTable;
     b := C.dd = new PageMap;
     b.degree = {};
     b.source = b.target = C;
     C)
ring Page := C -> C.ring
degree Page := C -> C.dd.degree


netPage = method()
netPage(Page,List,List) := (E,mins,maxs) -> (
    newmaxQ := maxs#1;
    newminQ := mins#1;
    newmaxP := maxs#0;
    newminP := mins#0;
    P := page E;
    L := select(keys P, i -> class i === List and P#i !=0);
    maxQ := max(apply(L, i -> i#1));
    minQ := min(apply(L, i -> i#1)); 
    maxP := max(apply(L, i -> i#0));
    minP := min(apply(L,i -> i#0));
    finalmaxQ := min {newmaxQ,maxQ};
    finalminQ := max {newminQ,minQ};
    finalmaxP := min {newmaxP,maxP};
    finalminP := max {newminP,minP}; 
    K := while finalmaxQ >= finalminQ list makeRow(finalmaxP, finalminP, finalmaxQ, P) do (finalmaxQ = finalmaxQ - 1);
   -- netList(K, Boxes => false)
   netList K
    )

net Page := E -> (
    L := select(keys E, i -> class i === List and E#i !=0);
    maxQ := max(apply(L, i -> i#1)); 
    minQ := min(apply(L, i -> i#1)); 
    maxP := max(apply(L, i -> i#0));
    minP := min(apply(L,i -> i#0));
    K := while maxQ >= minQ list makeRow(maxP, minP, maxQ, E) do maxQ = maxQ - 1;
   -- netList(K, Boxes => false)
   netList K
    )

makeRow = method()
makeRow(ZZ,ZZ,ZZ,Page) := (maxP,minP,q,E)->(L := {};
      apply(minP .. maxP, i-> 
	   if E#?{i,q} then L = append(L, stack(net E#{i,q}, "  ", net {i,q}))
	   else L = append(L, stack(net 0, " ", net {i,q})));
       L)

Page _ List := (E,L) -> ( if E#?L then E#L else (ring E)^0 )


spots Page := List => ( 
    P -> select(keys P, i -> class i === List and all(i, j -> class j === ZZ))
    )


page = method (Options => {Prune => false})

support Page := List => (
     P -> sort select (spots P, i -> P#i != 0))

-- at present there are no advanced constructors for page.

-- given {minP, maxP, Page} make a page.  the idea here is to make the needed keys
-- we then can make entries nonzero as needed.

-- this present method is mainly for testing code.  It might have other uses later. --
page(List,List,Page) := Page => opts -> (L,M,E) -> (
    if E.?ring then (
    minP := L#0;
    maxP := L#1;
    minQ := M#0;
    maxQ := M#1;
  --  E := new Page;
  --  E.ring = A;
    for i from minP to maxP do (
	for j from minQ to maxQ do (
	    E#{i,j} = (E.ring)^0;
    )
);
E) else error "page does not have a ring"
)


--------------------------------------------------------------------------------
-- PageMap
--------------------------------------------------------------------------------

PageMap = new Type of MutableHashTable
PageMap.synonym = "page map"
PageMap.GlobalAssignHook = globalAssignFunction
PageMap.GlobalReleaseHook = globalReleaseFunction
describe PageMap := d -> net expression d


spots PageMap := List => ( 
    d -> select(keys d, i -> class i === List and all(i, j -> class j === ZZ))
    )

support PageMap := List => (
     d -> sort select (spots d, i -> d#i != 0))


PageMap _ List := Matrix => (f,i) ->  if f#?i then f#i else (
      de := f.degree;
      so := (f.source)_i;
      ta := (f.target)_(i + de);
      map(ta,so,0))



lineOnTop := (s) -> concatenate(width s : "-") || s

net PageMap := f -> (
     v := between("",
	  apply(spots f, 
     	       i -> horizontalJoin(
		         net (i + f.degree), " : " , net (target f#i), " <--",
		         lineOnTop net f#i,
		         "-- ", net source f#i, " : ", net i
		    )
	       )
	       );
	  stack v
)

-- at present there are no constructors for pageMap


--------------------------------------------------------------------------------
-- spectral sequences 
--------------------------------------------------------------------------------
SpectralSequence = new Type of MutableHashTable
SpectralSequence.synonym = "spectral sequence"
SpectralSequence.GlobalAssignHook = globalAssignFunction
SpectralSequence.GlobalReleaseHook = globalReleaseFunction
describe SpectralSequence := E -> net expression E
net SpectralSequence := E -> (
  if hasAttribute(E, ReverseDictionary) 
  then toString getAttribute(E, ReverseDictionary) 
  else net expression E)
expression SpectralSequence := E -> stack(
  "  .-.  ", " (o o) ", " | O \\   Unnamed spectral sequence! ..ooOOOooooOO", 
  "  \\   \\  ", "   `~~~` ")


spectralSequence = method (Options =>{Prune => false})

spectralSequence FilteredComplex := SpectralSequence => opts -> K -> (
     new SpectralSequence from {
	  symbol filteredComplex => K,
	  symbol cache => CacheTable,
     	  symbol Prune => opts.Prune}
     )

SpectralSequence ^ InfiniteNumber:=
  SpectralSequence ^ ZZ := SpectralSequencePage => (E,r) -> (
      -- the case that r is an infinite number has been rewritten
      -- and also returns a page --- with no maps!
      -- this fixes an earlier bug.  
      if class r === InfiniteNumber then (
    if r < 0 then error "expected an infinite number bigger than zero"
    else (
	myList := {};
	K := E.filteredComplex;
	s := max K - min K + 1;
	H := new Page;
	-- again trying to handle the case of the zero complex --
    if min K_(infinity) < infinity and max K_infinity > - infinity then (
	    for p from min K to max K do (
	  	for q from - p + min K_(infinity) to max K_(infinity) do (
	       	    if E.Prune == false then H#{p,q} = epq(K,p,q,s)
	       	    else H#{p,q} = prune epq(K,p,q,s)
	       );
    	   ); 
       );
   ) ;
   
H
)
      else (
       if E#?r then E#r else (
       E#r = spectralSequencePage(E.filteredComplex,r, Prune => E.Prune);); 
       E#r
       )
       )

SpectralSequence _ InfiniteNumber :=
SpectralSequence _ ZZ := SpectralSequencePage => (E,r) -> ( E^r )

minimalPresentation SpectralSequence := prune SpectralSequence := SpectralSequence  => opts -> (E) -> (
	  spectralSequence(E.filteredComplex, Prune => true)
	  )

----------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- spectral sequence pages
--------------------------------------------------------------------------------
SpectralSequencePage = new Type of Page
SpectralSequencePage.synonym = "spectral sequence page"
SpectralSequencePage.GlobalAssignHook = globalAssignFunction
SpectralSequencePage.GlobalReleaseHook = globalReleaseFunction
describe SpectralSequencePage := E -> net expression E

spectralSequencePage = method (Options =>{Prune => false})

spectralSequencePage(FilteredComplex, ZZ) := SpectralSequencePage => opts ->  (K,r) -> ( 
new SpectralSequencePage from 
 {symbol filteredComplex=> K, 
       symbol Prune => opts.Prune,
       symbol number => r,
       symbol dd => spectralSequencePageMap(K,r, Prune => opts.Prune), 
       symbol cache => CacheTable}
  )

minimalPresentation SpectralSequencePage := prune SpectralSequencePage := SpectralSequencePage  => opts -> (E) -> (
     spectralSequencePage(E.filteredComplex, E.number, Prune => true)
     )

SpectralSequencePage _ List := Module => (E,i)-> ( source(E.dd _i) )
		    

SpectralSequencePage ^ List := Module => (E,i)-> (E_(-i))    

-- view the modules on a Spectral Sequence Page.  We are referring to these
-- as the support of the page.






page SpectralSequencePage := Page => opts -> E -> ( 
    	K := E.filteredComplex;
	s := E.number;
    H := new Page;
    -- again trying to handle the case of the zero complex --
    if min K_(infinity) < infinity and max K_infinity > - infinity then (
	    for p from min K to max K do (
	  	for q from -p + min K_(infinity) to max K_(infinity) do (
--		    H#{p,q} = E^s_{p,q}
	       	    if E.Prune == false then H#{p,q} = epq(K,p,q,s)
	       	    else H#{p,q} = prune epq(K,p,q,s)
	       )
	   );
       );
    H
    )

-- the following two methods are used to view the modules 
-- on the r th page in grid form.  
-- this method is called in net of spectral sequence page.
-- it would be good to delete the zero rows.

net SpectralSequencePage := E -> (page E)

support SpectralSequencePage := E -> (
     new Page from apply(spots E.dd, i-> i=> source E.dd #i) )


------------------------------------------------------------------------
-- below are the methods which compute the
-- individual terms on a page of a spectral sequence
-- WE ARE USING HOMOLOGICAL INDEXING CONVENTIONS.
---------------------------------------------------------------------
-- By default the maximum integer key
-- of the filtered complex corresponds to the ambient complex.
-- This is used in the formulas below.
-- the formulas below are the homological versions of the ones in I.2.4 of Danilov's 
-- treatment of spectral sequences in Shafarevich's Encyclopedia of 
-- Math Algebraic Geometry II.  
-- In any event it is easy enough to prove directly that they satisfy the requirements 
-- for a spectral sequence.

cycles := (K,p,q,r) -> (
ker inducedMap((K_infinity)_(p+q-1) / K_(p-r) _ (p+q-1), 
     K_p _ (p+q), K_(infinity).dd_(p+q), Verify => false))

boundaries := (K,p,q,r) -> (
    ( image (K_(p+r-1).dd_(p+q+1))) + (K_(p-1) _ (p+q)))

-- compute the pq modules on the rth page
epq = method()
epq(FilteredComplex,ZZ,ZZ,ZZ) := (K,p,q,r) -> (
    ((cycles(K,p,q,r) + boundaries(K,p,q,r)) / boundaries(K,p,q,r)))

-- the pq maps on the rth page.
epqrMaps = method()
epqrMaps(FilteredComplex,ZZ,ZZ,ZZ) := (K,p,q,r) -> (
     inducedMap(epq(K,p-r,q+r-1,r), epq(K,p,q,r),(K_infinity).dd_(p+q), Verify => false))


-- prune the pq maps on the rth page. --
--  "sourcePruningMap",
-- "targetPruningMap"
--- the following could be replaced by prune d --- except I want to cache the 
-- pruning maps.  --

pruneEpqrMaps = method()
pruneEpqrMaps(FilteredComplex,ZZ,ZZ,ZZ) := (K,p,q,r) -> ( 
     d := epqrMaps(K,p,q,r);
     N := minimalPresentation(source d);
     M := minimalPresentation(target d);
     f := inverse(M.cache.pruningMap)* d * (N.cache.pruningMap);
     f.cache #(symbol sourcePruningMap) = N.cache.pruningMap;
     f.cache #(symbol targetPruningMap) = M.cache.pruningMap;
     f 
     )

ErMaps = method(Options => {Prune => false})
ErMaps(FilteredComplex,ZZ,ZZ,ZZ) := Matrix => opts -> (K,p,q,r) -> (if opts.Prune == false then
     epqrMaps(K,p,q,r)
     else   pruneEpqrMaps(K,p,q,r))

-- the homology at the pq spot on the rth page.
rpqHomology = method()
rpqHomology(SpectralSequence,ZZ,ZZ,ZZ) := (E,p,q,r) -> (
      (ker(E^r .dd_{p,q})) / (image(E^r .dd_{p+r,q-r+1}) )
      )

-- the isomorphism of the homology at the pq spot
-- on the r-th page and the module on at the pq spot on the r+1-th page.
homologyIsomorphism = method()
homologyIsomorphism(SpectralSequence,ZZ,ZZ,ZZ) := (E,p,q,r) -> (
    if E.Prune == false then 
inducedMap(source (E^(r+1) .dd_{p,q}),rpqHomology(E,p,q,r), id_(E^(r+1) .filteredComplex _infinity _(p+q)), Verify=>false) -- if Verify not set to false can get error when running on M2 1.9
    else
    rpqPruneIsomorphism(E,p,q,r)
  ) 

rpqPruneIsomorphism = method()
rpqPruneIsomorphism(SpectralSequence,ZZ,ZZ,ZZ) := (E,p,q,r) -> (    
    M := rpqHomology(E,p,q,r);
    f := inducedMap(target (E^(r + 1) .dd_{p,q}) .cache.sourcePruningMap,
	    M, (E^r .dd_{p,q}).cache.sourcePruningMap, Verify=>false); -- if Verify not set to false can get error when running on M2 1.9
	inverse((E^(r + 1) .dd_{p,q}) .cache.sourcePruningMap) * f    
  ) 

---
-- Spectral Sequence Page Maps
---

SpectralSequencePageMap = new Type of PageMap
SpectralSequencePageMap.synonym = "spectral sequence page map"
SpectralSequencePageMap.synonym = "spectral sequence page map"
SpectralSequencePageMap.GlobalAssignHook = globalAssignFunction
SpectralSequencePageMap.GlobalReleaseHook = globalReleaseFunction
describe SpectralSequencePageMap := d -> net expression d



spectralSequencePageMap = method(Options =>{Prune => false})

spectralSequencePageMap(FilteredComplex,ZZ) := SpectralSequencePageMap => opts ->
 (K,r) -> (myList:={};
     -- try to handle case coming from the zero complex --
     Kmin := min K_infinity; Kmax := max K_(infinity);
     if class Kmin < infinity  and Kmax > - infinity then (
           for p from min K to max K do (
		for q from -p + min K_(infinity) to max K_(infinity) -p do (
	       	     myList = 
		     append(myList, {p,q} => ErMaps(K,p,q,r, Prune => opts.Prune)) )); );
	       new SpectralSequencePageMap from 
	       join (myList, {symbol cache =>  new CacheTable,
		    symbol degree => {-r,r-1}, 
		    symbol filteredComplex => K, 
		    symbol Prune => opts.Prune})
      )


SpectralSequencePageMap _ List := Matrix => (d,i)-> (if (d)#?i then d#i 
     	  else  
	       if d.Prune == false then 
	            epqrMaps(d.filteredComplex,i#0,i#1,- d.degree #0) 
     	       else
	       	    pruneEpqrMaps(d.filteredComplex,i#0,i#1,- d.degree #0) 	       	    		    
		    )

SpectralSequencePageMap ^ List := Matrix => (d,i)-> (d_(-i))    


-- auxiliary spectral sequence stuff.  

filteredComplex SpectralSequence := FilteredComplex => opts -> E -> E.filteredComplex
chainComplex SpectralSequence := ChainComplex => E -> chainComplex filteredComplex E
-- given a morphism f: A --> B
-- compute the connecting map
-- HH_{n+1}( coker f) --> HH_n (im f)

connectingMorphism = method()

connectingMorphism(ChainComplexMap,ZZ) := (a,n) -> (
    K := filteredComplex ({a}) ;
    e := spectralSequence K ;
    e^1 .dd_{1, n}
    )
-- here are some needed functions related to Hilbert polynomials --
hilbertPolynomial ZZ := ProjectiveHilbertPolynomial => o -> (M) -> ( if M == 0
    then new ProjectiveHilbertPolynomial from {} else
    new ProjectiveHilbertPolynomial from {0 => M}
    )
ProjectiveHilbertPolynomial == ZZ := (M,N) -> (M == hilbertPolynomial N)
ProjectiveHilbertPolynomial + ZZ := (P, N) -> P + hilbertPolynomial N
ZZ + ProjectiveHilbertPolynomial := (P,N) -> hilbertPolynomial P + N
ProjectiveHilbertPolynomial - ZZ := (P, N) -> P - hilbertPolynomial N
ZZ - ProjectiveHilbertPolynomial := (P,N) -> hilbertPolynomial P - N
---

hilbertPolynomial (SpectralSequencePage) := Page => o -> (E) -> (
    P := new Page;
    apply(spots E .dd, i -> P#i = hilbertPolynomial(E_i));
    P
    )

pruningMaps = method()
pruningMaps(SpectralSequencePage) := (E) -> ( if E.Prune == false then error "page is not pruned"
    else
    P := new PageMap;
    P.degree = E.dd.degree;
    apply(spots E.dd, i -> P#i = E.dd_i .cache.sourcePruningMap);
    P    
    )

basis (ZZ,SpectralSequencePage) := opts -> (deg,E) -> (
    P := new Page;
    apply(spots E.dd, i -> P#i = basis(deg,E_i));
    P
    )

basis (List,SpectralSequencePage) := opts -> (deg,E) -> (
    P := new Page;
    apply(spots E.dd, i -> P#i = basis(deg,E_i));
    P
    )
--
--
--

edgeComplex = method()

edgeComplex(SpectralSequence) := (E) -> (
    if E.Prune == true then error "not currently implemented for pruned spectral sequences";
   if E.Prune == true then error "not currently implemented for pruned spectral sequences";
    M := select(spots E^2 .dd, i -> E^2_i != 0);
    l := min apply(M, i -> i#0);
    m := min apply(M, i -> i#1);
    C := chainComplex E;
    if M != {} then (
    chainComplex {inducedMap(E^2_{l + 1, m}, HH_(l + m + 1) C, id_(C_(l + m + 1))),
    inducedMap(HH_(l + m + 1) C, E^2_{l,m + 1}, id_(C_(l + m + 1))), 
    E^2 .dd_{l + 2,m}, inducedMap(E^2_{l + 2, m}, HH_(l + m + 2) C, id_(C_(l + m + 2)))})
    else
    (c := new ChainComplex; c.ring = E.filteredComplex _infinity .ring;
    c)
    )

 
filteredHomologyObject = method()

filteredHomologyObject(ZZ, ZZ,FilteredComplex) := (p,n,K) -> (
    image(inducedMap(HH_n K_infinity, HH_n K_p, id_(K_infinity _n)))
    )


associatedGradedHomologyObject = method()

associatedGradedHomologyObject(ZZ,ZZ,FilteredComplex) := (p,n,K) -> (
    filteredHomologyObject(p,n,K) / filteredHomologyObject(p-1,n,K)
    )



-----------------------------------------------------------
-----------------------------------------------------------



beginDocumentation()

undocumented {
 --   page,  
--    (degree, Page),
--    (net, FilteredComplex),
--    (net, Page),
--    (net, PageMap),
--    (net, SpectralSequencePage),
--    (net, SpectralSequence),
--    (symbol _, Page, List),
--    (page, SpectralSequencePage),
--    (symbol _, PageMap, List),
--    (ring, Page),
--    (spectralSequencePageMap, FilteredComplex, ZZ),
--    (spots, PageMap),
--    (support, SpectralSequencePage),
 --  ReducedHomology, 
--sourcePruningMap, targetPruningMap,
--   pageMap,
--      ErMaps,
--      (ErMaps,FilteredComplex, ZZ, ZZ, ZZ),
--      (support, PageMap),
--      (page,List, List, Page),
--   (expression, SpectralSequence),
--   spectralSequencePageMap,
--   Shift,
--   (support, FilteredComplex)
    }


document {
  Key => {
    Shift
  },
  Headline => "name for an optional argument"
}

document {
  Key => {
    ReducedHomology
  },
  Headline => "name for an optional argument"
}


doc ///
          Key
           targetPruningMap
///


doc ///
          Key
           sourcePruningMap
///



doc ///
          Key
           [spectralSequence,Prune]

///

doc ///
          Key
           [spectralSequencePage,Prune]

///

doc ///
          Key
           [filteredComplex, ReducedHomology]
///

doc ///
          Key
           [filteredComplex, Shift]
///

doc ///
          Key
           [page,Prune]
///

doc ///
          Key
            [spectralSequencePageMap,Prune]
///



document { 
  Key => SpectralSequences,
  Headline => "a package for working with filtered complexes and spectral sequences",
   "Spectral sequences, although notoriously technical, are very useful in applications---especially when they degenerate quickly.
   By contrast, little is known about their general structure when they fail to degenerate quickly.  Even in cases when the terms in the spectral sequences are well understood, the maps remain mysterious.
   One of the motivations behind this package is to shed light on spectral sequences through examples.  Its purpose
   is to allow for effective calculations of particular kinds of spectral sequences.
   As one general situation, which illustrates some capabilities of this package,
   let k be a computable field, S a k-algebra of finite type, C a bounded chain complex of
 finitely generated S-modules, and FC a bounded ascending filtration of C.  This package is
 capable of computing, under these assumptions, the spectral sequence---especially the differentials on each page---determined by FC.
 ", 
 -- SUBSECTION "Contributors",
 -- "The following people have generously contributed code or worked on our code.",
 -- UL {
   -- {HREF("","")},
   -- {HREF("","")},
   -- {HREF("","")},
   -- {HREF("","")},
   -- {HREF("","")},},
   SUBSECTION "Constructors used in this package",
   UL {
    TO "How to make filtered complexes from chain complex maps", --"How to work with filtered complexes", --"Making filtered chain complexes from chain complex maps",
    TO "Filtrations and tensor product complexes",
    TO "Filtrations and homomorphism complexes",
    TO "Filtered complexes and simplicial complexes",
    TO "I-adic filtrations of chain complexes and their spectral sequences",
  --  TO "Spectral sequences from filtered chain complexes"
    },
    
 
  SUBSECTION "Other examples which illustrate this package",
  UL {
    TO "Computing the Serre Spectral Sequence associated to a Hopf Fibration",
    TO "Balancing Tor",
    TO "Spectral sequences and hypercohomology calculations",
    TO "Spectral sequences and connecting morphisms",
    TO "Spectral sequences and non-Koszul syzygies",
    TO "A spectral sequence which fails to degenerate quickly",
    TO "Seeing Cancellations",
    TO "Edge homomorphisms",
    TO "Examples of change of rings Spectral Sequences"
  },

  SUBSECTION "More easy topological examples",
  UL { TO "Identifying anti-podal points of the two sphere", --"The quotient map SS ^2 --> RR PP ^2",--"More topological examples",
    TO "The fibration of the Klein Bottle over the sphere with fibers the sphere", --"The fibration SS^1 --> Klein Bottle --> SS^1",
    TO "The trivial fibration over the sphere with fibers the sphere"}, -- SS^1 --> SS^1 x SS^1 --> SS^1"},
}  


doc ///
     Key
     	  "Examples of filtered complexes and spectral sequences"
     Headline
     	  How to use this package
     Description
     	  Text
	       Here is a list of some examples which illustrate various parts of this package.
	       
	       {\bf First examples which show how to use this package}
    	       
	       $\bullet$ @TO"How to make filtered complexes from chain complex maps"@
    	       
	       $\bullet$ @TO"Filtrations and tensor product complexes"@
    	       
	       $\bullet$ @TO"Filtrations and homomorphism complexes"@
    	       
	       $\bullet$ @TO"Filtered complexes and simplicial complexes"@
    	       
	       $\bullet$ @TO"I-adic filtrations of chain complexes and their spectral sequences"@
    
 
               {\bf More elaborate examples which illustrate this package}

               $\bullet$ @TO"Computing the Serre Spectral Sequence associated to a Hopf Fibration"@
    	       
	       $\bullet$ @TO"Balancing Tor"@
	       
	       $\bullet$ @TO"Spectral sequences and hypercohomology calculations"@
    	       
	       $\bullet$ @TO"Spectral sequences and connecting morphisms"@
    	       
	       $\bullet$ @TO"Spectral sequences and non-Koszul syzygies"@
	       
	       $\bullet$ @TO"Seeing Cancellations"@
    	       
	       $\bullet$ @TO"A spectral sequence which fails to degenerate quickly"@
	       
	       $\bullet$ @TO"Edge homomorphisms"@
	       
	       $\bullet$ @TO"Examples of change of rings Spectral Sequences"@
 
               {\bf More easy topological examples}
	       
	       $\bullet$ @TO"Identifying anti-podal points of the two sphere"@
	       
	       $\bullet$ @TO"The fibration of the Klein Bottle over the sphere with fibers the sphere"@ 
	       
	       $\bullet$ @TO"The trivial fibration over the sphere with fibers the sphere"@
	       
 
 	  	  
///

doc ///
     Key
     	  "I-adic filtrations of chain complexes and their spectral sequences"
     Description
     	 Text
	      By multiplying a chain complex by successive powers of an ideal we obtain a filtered complex.  
	 Example     
	      B = QQ[a..d]
	      J = ideal vars B
	      C = complete res monomialCurveIdeal(B,{1,3,4})
	      K = filteredComplex(J,C,4)
	 Text
	      Here are some higher pages of the associated spectral sequence:
	 Example
	       E = prune spectralSequence K
--	       E^2
--	       E^3
--	       E^3 .dd
	       E^4
	       E^4 .dd
///


doc ///
     Key
     	"Filtered complexes and simplicial complexes"
     Description
          Text
	    We can make a filtered complex from a nested list of simplicial 
     	    complexes:
     	  Example
	      A = QQ[x,y,z,w];	     
	      F2D = simplicialComplex {x*y*z, w*z};
	      F1D = simplicialComplex {x*y, w};
	      F0D = simplicialComplex {x,w};
	      K = filteredComplex{F2D, F1D, F0D}
	  Text
	      The resulting spectral sequence takes the form:
	  Example
	      E = prune spectralSequence K;
	      E^0
	      E^0 .dd
	      E^1
	      E^1 .dd
	      E^2 
	      E^2 .dd
	      E^infinity    
	  Text
     	     If we want the homology of the complex to be the non-reduced homology
     	     of the simplicial complex we set the ReducedHomology option to false:
     	  Example 
	     k = filteredComplex({F2D, F1D, F0D}, ReducedHomology => false)
	  Text
	      The resulting spectral sequence takes the form:
	  Example    
	      e = prune spectralSequence k;
	      e^0
	      e^0 .dd
	      e^1 .dd
	      e^2
	      e^2 .dd
	      e^infinity
     SeeAlso
     	  "How to make filtered complexes from chain complex maps"
	  "Filtrations and tensor product complexes"
	  "Filtrations and homomorphism complexes"
///


doc ///
     Key
        "Filtrations and homomorphism complexes"
     Description
     	  Text
	     Let $S$ be a commutative ring and let 
	     $B : \dots \rightarrow B_{i} \rightarrow B_{i - 1} \rightarrow \cdots $ and
	     $C : \dots \rightarrow C_{i} \rightarrow C_{i - 1} \rightarrow \cdots $ be chain complexes.
    	     
	     For all integers $p$ and $q$ let $K_{p,q} := Hom_S(B_{-p}, C_q)$, 
	     let $d'_{p,q} : K_{p,q} \rightarrow K_{p - 1, q}$ denote the homorphism
	     $ \phi \mapsto \partial^B_{-p + 1}  \phi$, and let
	     $d^{''}_{p,q} : K_{p,q} \rightarrow K_{p, q - 1} $ denote the homorphism
	     $\phi \mapsto (-1)^p \partial^C_q  \phi$.
    	    	
             The chain complex $Hom(B, C)$ is given by 
	     $ Hom(B, C)_k := \prod_{p + q = k} Hom_S(B_{-p}, C_q) $ 
	     and the differentials  
	     by $ \partial := d^{'} + d^{''} $;
	     it carries two natural ascending filtrations $F' ( Hom(B, C) )$ and $F''( Hom(B, C))$.  

             The first is obtained by 
	     letting $F'_n (Hom(B, C))$ be the chain complex determined by setting
	     $F'_n (Hom(B, C))_k := \prod_{p + q = k , p \leq n} Hom_S(B_{-p}, C_q)$
	     and the differentials $\partial := d' + d''$.

             The second is obtained by letting $F''_n (Hom(B, C)) := \prod_{p + q = k , q \leq n} Hom_S(B_{-p}, C_q)$
	     and the differentials $\partial := d' + d''$.
    	     
             In {\it Macaulay2}, using this package, $F'$ and $F''$ as defined above are
	     computed as illustrated in the following example, by using 
	     Hom(filteredComplex B, C) or Hom(B,filteredComplex C).
	     
	 Example
	     A = QQ[x,y,z,w];
	     B = res monomialCurveIdeal(A, {1,2,3});
	     C = res monomialCurveIdeal(A, {1,3,4});
	     F' = Hom(filteredComplex B, C)
	     F'' = Hom(B,filteredComplex C)
	 Text
	     Notice that the display above shows that these are different filtered complexes.
	     The resulting spectral sequences take the form:
	 Example
	     E' = prune spectralSequence F';
	     E'' = prune spectralSequence F'' ;
	     E' ^0
	     E' ^ 0 .dd
	     E'' ^0
	     E'' ^1    
///

doc ///
     Key
        "Filtrations and tensor product complexes"
     Description
     	  Text
	    Let $S$ be a commutative ring and let 
	    $B : \dots \rightarrow B_{i} \rightarrow B_{i - 1} \rightarrow \dots $ and
	    $C : \dots \rightarrow C_{i} \rightarrow C_{i - 1} \rightarrow \dots $ be chain complexes.
	    
	    For all integers $p$ and $q$ let $K_{p,q} := B_p \otimes_S C_q$, let $d'_{p,q} : K_{p,q} \rightarrow K_{p - 1, q}$ 
	    denote the homorphism 
	    $\partial^B_{p} \otimes 1$, and let $d''_{p,q} : K_{p,q} \rightarrow K_{p, q - 1} $ denote the 
	    homorphism $(-1)^p \otimes \partial_q^C $.
    
            The chain complex $B \otimes_S C$ is given by
	    $ (B \otimes_S C)_k := \oplus_{p + q = k} B_p \otimes_S C_q$
	    and the differentials by $\partial := d' + d''$. It carries two natural ascending filtrations 
	    $F'B \otimes_S C$ and $F'' B \otimes_S C$.  
    	    
	    The first is obtained by letting
	    $F'_n (B \otimes_S C)$ be the chain complex determined by setting
	    $F'_n (B \otimes_S C)_k := \oplus_{p + q = k , p \leq n} B_{p} \otimes_S C_q$
	    and the differentials $\partial := d' + d''$.
    	    
	    The second is obtained by letting
	    $F''_n (B \otimes_S C)$ be the chain complex determined by setting
	    $F''_n (B \otimes_S C)_k := \oplus_{p + q = k , q \leq n} B_{p} \otimes_S C_q$
	    and the differentials $\partial := d' + d''$.

            In Macaulay2 we can compute these filtered complexes as follows.  
	    --To obtain the chain complex $F' B \otimes_S C$ we use the syntax 
	    --$(filteredComplex B)\otimes C$. 
	    --To obtain the chain complex $ F'' B \otimes_S C$ we use the syntax 
	    --$ B\otimes(filteredComplex C)$.
    	  Example
	      A = QQ[x,y,z,w];
	      B = res monomialCurveIdeal(A,{1,2,3});
	      C = res monomialCurveIdeal(A,{1,3,4});
	      F' = (filteredComplex B) ** C
	      F'' = B ** (filteredComplex C)  
	 Text
	     The pages of the resulting spectral sequences take the form:
	 Example
	     E' = prune spectralSequence F';
	     E'' = prune spectralSequence F'';
	     E' ^0
	     E' ^ 1
	     E'' ^0
	     E'' ^1         
     SeeAlso   
	  "Balancing Tor"	     
///  
   
doc ///
     Key
        "How to make filtered complexes from chain complex maps"
   --  Headline
     --	  the most primitive way to make filtered complexes
     Description
     	  Text  
       	    We describe
	    the most primitive way to create filtered complexes.
	    
	    Let $C$ be a chain complex and consider a list of
	    chain complex maps $\{\phi_n, \phi_{n - 1}, \dots, \phi_0  \}$ 
	    with properties that $C$ is the target of $\phi_i$, for $0 \leq i \leq n$, and the
	    image of $\phi_{i-1}$ is a subchain complex of the image of $\phi_i$, for $1 \leq i \leq n$.
	    Given this input data we produce an ascending filtered chain complex $FC$
	    with the properties that $F_k C = C$ for $k \geq n + 1$ and $F_k C = image \phi_k$, for $k = 0, \dots, n$.
	    
	    We now illustrate how this is done in two easy examples.
	    We first make three chain complexes $C$, $D$, and $E$, 
	    two chain complex maps, $d : D \rightarrow C$ 
	    and $e : E \rightarrow C$, and then
	    compute the resulting filtration of $C$.
--	    When then consider a boundary case by considering the filtered complex obtained
--	    from a single chain complex map, that is the identity of $C$.
     	  Text
	     Let's make our chain complexes $C$, $D$, and $E$.	     
     	  Example	       	 
	       R = QQ[x,y,z,w] ;
	       c2 = matrix(R,{{1},{0}}) ;
	       c1 = matrix(R,{{0,1}}) ;
	       C = chainComplex({c1,c2})        
	       D_2 = image matrix(R,{{1}});
	       D_1 = image matrix(R,{{1,0},{0,0}});
	       D_0 = image matrix(R,{{1}});
	       D = chainComplex({inducedMap(D_0,D_1,C.dd_1),inducedMap(D_1,D_2,C.dd_2)})     
               E_2 = image matrix(R,{{0}});
	       E_1 = image matrix(R,{{1,0},{0,0}});
	       E_0 = image matrix(R,{{1}});
	       E = chainComplex({inducedMap(E_0,E_1,C.dd_1),inducedMap(E_1,E_2,C.dd_2)})
     	  Text
	       We now make our chain complex maps.
     	  Example	       	     
	       d = chainComplexMap(C,D,apply(spots C, i-> inducedMap(C_i,D_i,id_C _i)))
	       e = chainComplexMap(C,E,apply(spots C, i->inducedMap(C_i,E_i, id_C _i)))
	  Text
	       We can check that these are indeed chain complex maps:
	  Example   
	       isChainComplexMap d
	       isChainComplexMap e
     	  Text 
	       Now, given the list of chain complex maps $\{d, e\}$, we obtain
	       a filtration of $C$ by:
     	  Example	       	       
	       K = filteredComplex({d,e})
	  Text
	     If we want to specify a minimum filtration degree
             we can use the Shift option.
      	  Example	       	     
	       L = filteredComplex({d,e},Shift =>1)
	       M = filteredComplex({d,e},Shift =>-1)
--	  Text
--	     We now explain a boundary case in which the list consists of a single map $\{\phi_0\}$.
--	  Example
--	      P = filteredComplex {id_C}   
--    	      P_1	  
///	  

---
-- Examples
---

doc ///
    Key
      "A spectral sequence which fails to degenerate quickly"
   -- Headline
     --	  nonzero maps on higher page numbers
    Description
    	  Text
	       The following example is taken from p. 127, Fig 7.2 of 
	       Zomorodian's {\it Topology for computing}.  In that figure, a filtration of a suitable
	       simplicial complex is pictured.  Here we compute the associated spectral sequence.
	       As we will see below, the spectral sequences has nonzero maps on higher page numbers.
     	  Example
		A = ZZ [s,t,u,v,w] ;
		D0 = simplicialComplex {s} ;
		D1 = simplicialComplex {s,t} ;
		D2 = simplicialComplex {s,t,u} ;
		D3 = simplicialComplex {s*t, u} ;
		D4 = simplicialComplex {s*t, u, v} ;
		D5 = simplicialComplex {s*t, u, v, w} ;
		D6 = simplicialComplex {s*t, s*w ,u, v} ;
		D7 = simplicialComplex {s*t, s*w ,t * w, u, v} ;
		D8 = simplicialComplex {s*t, s*w ,t * w, u * v} ;
		D9 = simplicialComplex {s*t, s*w ,t * w, u * v, s * v} ;
		D10 = simplicialComplex {s*t, s*w ,t * w, u * v, s * v, s*u} ;
		D11 = simplicialComplex {s*t, s*w ,t * w, u * v, s * v, s*u, u * w} ;
		D12 = simplicialComplex {s*t, s*w ,t * w, u * v, s * v, s*u, u * w, t* u} ;
		D13 = simplicialComplex {s*t, s*w ,t * w, u * v, s * v, s*u, u * w, t* u, t*u*w} ;
		D14 = simplicialComplex {s*t, s*w ,t * w, u * v, s * v, s*u, u * w, t* u, t*u*w, s*u*w} ;
		D15 = simplicialComplex {s*t, s*w ,t * w, u * v, s * v, s*u, u * w, t* u, t*u*w, s*u*w,s*t*u} ;
		D16 = simplicialComplex {s*t, s*w ,t * w, u * v, s * v, s*u, u * w, t* u, t*u*w, s*u*w,s*t*u, s*u*v} ;
		D17 = simplicialComplex {s*t, s*w ,t * w, u * v, s * v, s*u, u * w, t* u, t*u*w, s*u*w,s*t*u, s*u*v, s*t*w} ;
		L = reverse {D0, D1, D2, D3, D4, D5, D6, D7, D8, D9, D10, D11, D12, D13, D14, D15, D16, D17} ;
		K = filteredComplex (L, ReducedHomology => false) ;
		E = prune spectralSequence K ;
		E^0
		E^1 .dd
		E^8
		E^8 .dd
		E^9
		E^9 .dd
		E^infinity
		prune HH K_infinity
///

doc ///
    Key
      "Seeing Cancellations"
   -- Headline
     --	  nonzero maps on higher page numbers
    Description
    	  Text
	       Here we give an example of a spectral sequence that takes n+2 steps to degenerate, where
	       n is the embedding dimension of the ring.  We present this when n = 2 but the user with 
	       computational power can easily do a bigger case. 	       
     	  Example
    	       S = ZZ/101[x,y];
	       I = ideal(x^2,x*y,y^2);
	       R = S/I;
	       kR = coker vars R;
	       kS = coker vars S;
	       CS = res kS;
	       CR = res(kR,LengthLimit=>6);
	       CS' = CS**R;
	       E = prune spectralSequence (CS' ** filteredComplex CR);
    	  Text
	       Since this spectral sequence only consists of $k$ vector spaces, and all are generated
	       in a single degree, for ease of presentation we may as well just look at the rank and degree
	       which we can easily encode in a matrix with $rt^d$ encoding the rank $r$ and degree $d$ of each
	       vector space $E_{i,j}$.
    	  Example
               use ZZ[t]
    	       easyPresentation = (P,n,m) -> (
		  transpose matrix apply(n, 
		      i-> apply(m, 
			  j-> (length (P_{i,j}))*t^(
			      if (L = unique flatten degrees P_{i,j})!= {} then first L else 0)
			  )
		      ));
          Text
	       To see what we're going for, we compute the E_{infinity} page and also some earlier pages.  
	       Notice that it's clear that all terms except those in the top row of the matrix must eventually
	       disappear, but for this to happen, there must a map of the right degree mapping to them.
          Example	       
	       easyPresentation(E_infinity,6,3)
	       easyPresentation(E_1,6,3)
	       easyPresentation(E_2,6,3)
	       easyPresentation(E_3,6,3)
               length image ((E_2).dd_{3,0})
	       length image (E_3).dd_{3,0}
	  Text 
	       The final two computations are meant to explain that the copy of $k^8$ in degree 3 that 
	       appears on the $E_1$ cancels in two steps via an $E_2$ map with $k^6$ and via an $E_3$ map with a $k^2$.
///

doc ///
    Key
      "Identifying anti-podal points of the two sphere"
    Description
    	  Text
	      In this example we compute the spectral sequence arising from
	      the quotient map
	      $\mathbb{S}^2 \rightarrow \mathbb{R} \mathbb{P}^2$, 
	      given by identifying anti-podal points. 
	      This map can be realized by a simplicial map along the lines of Exercise 27, Section 6.5 of Armstrong's
	      book {\it Basic Topology}.
	      In order to give a combinatorial picture of the quotient map
	      $\mathbb{S}^2 \rightarrow \mathbb{R} \mathbb{P}^2$, 
	      given by identifying anti-podal points, we
 	      first make an appropriate simplicial realization of $\mathbb{S}^2$.
	      Note that we have added a few barycentric coordinates.
     	  Example
	      S = ZZ[v1,v2,v3,v4,v5,v6,v15,v12,v36,v34,v46,v25];
	      twoSphere = simplicialComplex {v3*v4*v5, v5*v4*v15, v15*v34*v4, v15*v34*v1, v34*v1*v6, v34*v46*v6, v36*v46*v6, v3*v4*v46, v4*v46*v34, v3*v46*v36, v1*v6*v2, v6*v2*v36, v2*v36*v12,v36*v12*v3, v12*v3*v5, v12*v5*v25, v25*v5*v15, v2*v12*v25, v1*v2*v25, v1*v25*v15};	   
	  Text
	     We can check that the homology of the simplicial complex twoSphere agrees with that of $\mathbb{S}^2$.
	  Example
	      C = truncate(chainComplex complex twoSphere,1)	
	      prune HH C
	  Text
	      We now write down our simplicial complex whose topological realization 
	      is $\mathbb{R} \mathbb{P}^2$.
	  Example     
	      R = ZZ[a,b,c,d,e,f];
	      realProjectivePlane = simplicialComplex {a*b*c, b*c*d, c*d*e, a*e*d, e*b*a, e*f*b, d*f*b, a*f*d, c*f*e,a*f*c};
	  Text 
	      Again we can check that we've entered a simplicial complex
       	      whose homology agrees with that of the real projective plane.
	  Example
	      B = truncate(chainComplex complex realProjectivePlane,1)	 
	      prune HH B
    	  Text
	      We now compute the fibers of the anti-podal quotient map
 	      $\mathbb{S}^2 \rightarrow  \mathbb{R} \mathbb{P}^2$.
	      The way this works for example is:
	      $a = v3 ~ v1, b = v6 ~ v5, d = v36 ~ v15, c = v4 ~ v2, 
	      e = v34 ~ v12, f = v46 ~ v25$

              The fibers over the vertices of $\mathbb{R} \mathbb{P}^2$ are:
	 Example     
	      F0twoSphere = simplicialComplex {v1,v3,v5,v6, v4,v2, v36,v15, v34,v12, v46,v25}
    	 Text
	      The fibers over the edges of $\mathbb{R}\mathbb{P}^2$ are: 
   	 Example     
	      F1twoSphere = simplicialComplex {v3*v4, v1*v2,v3*v5, v1*v6,v4*v5, v2*v6, v5*v15, v6*v36, v4*v34, v2*v12, v15*v34, v36*v12, v1*v15, v3*v36, v46*v34, v25*v12, v6*v34, v5*v12, v6*v46, v5*v25, v36*v46, v15*v25, v3*v46, v1*v25, v4*v15, v2*v36, v1*v34, v3*v12, v4*v46, v25*v2}
	 Text
	      The fibers over the faces is all of $\mathbb{S}^2$.
	 Example     
	      F2twoSphere = twoSphere
	 Text
	      The resulting filtered complex is:
	 Example
	      K = filteredComplex({F2twoSphere, F1twoSphere, F0twoSphere}, ReducedHomology => false) 
	 Text
	      We now compute the resulting spectral sequence.
    	 Example
	      E = prune spectralSequence K
	      E^0
	      E^1
	      E^0 .dd
	      E^1 .dd
	      E^2
	      E^2 .dd
///

doc///
    Key
      "The fibration of the Klein Bottle over the sphere with fibers the sphere"
    Description
    	 Text
	      In this example we give a simplicial realization of the fibration 
	      $\mathbb{S}^1 \rightarrow {\rm Klein Bottle} \rightarrow \mathbb{S}^1$.  
	      To give a simplicial realization of this fibration we first make a simplicial
	      complex which gives a triangulation of the Klein Bottle.
	      The triangulation of the Klein Bottle that we use has 18 facets and is, up to relabling, the triangulation of the Klein bottle given
	      in Figure 6.14 of Armstrong's book {\it Basic Topology}.
    	 Example
	      S = ZZ[a00,a10,a20,a01,a11,a21,a02,a12,a22];
	      -- there will be 18 facets of Klein Bottle
	      Delta = simplicialComplex {a00*a10*a02, a02*a12*a10, a01*a02*a12, a01*a12*a11, a00*a01*a11, a00*a11*a10, a10*a12*a20, a12*a20*a22, a11*a12*a22, a11*a22*a21, a10*a11*a21, a10*a21*a20, a20*a22*a00, a22*a00*a01, a21*a22*a01, a21*a02*a01, a20*a21*a02, a20*a02*a00}
 	 Text
	      We can check that the homology of this simplicial complex agrees with that
	      of the Klein Bottle:
	 Example     
	      C = truncate(chainComplex complex Delta,1)
	      prune HH C
    	 Text
	      Let $S$ be the simplicial complex with facets $\{A_0 A_1, A_0 A_2, A_1 A_2\}$.  Then $S$ is a triangulation of $S^1$.  The simplicial map
	      $\pi : \Delta \rightarrow S$ given by $\pi(a_{i,j}) = A_i$ is a combinatorial realization of the fibration
	      $S^1 \rightarrow {\rm Klein Bottle} \rightarrow S^1$.
	      The subsimplicial complexes of $\Delta$, which arise from the 
	      the inverse images of the simplicies of $S$, are described below.
	 Example     
	      F1Delta = Delta
	      F0Delta = simplicialComplex {a00*a01,a01*a02,a00*a02,a10*a11,a10*a12,a11*a12,a21*a20,a20*a22,a21*a22}
    	 Text
	      The resulting filtered chain complex is:  
	 Example
	      K = filteredComplex({F1Delta, F0Delta}, ReducedHomology => false)
    	Text
	      The resulting spectral sequence is:
	Example      
	      E = prune spectralSequence K
	      E^0
	      E^0 .dd
	      E^1
	      E^1 .dd
	      E^2
    	Text
	      Note that the spectral sequence is abutting to what it should --- the integral
	      homology of the Klein bottle
///

doc ///
    Key
      "The trivial fibration over the sphere with fibers the sphere"--"The trivial fibration over the sphere with fiber the sphere"
    Description
         Text
	      In this example we compute the spectral sequence associated to the 
	      trivial fibration $\mathbb{S}^1 \rightarrow  \mathbb{S}^1 x \mathbb{S}^1 \rightarrow  \mathbb{S}^1$,
	      where the map is given by one of the projections.  To give a simplicial realization of this fibration we first make a simplicial complex
	      which gives a triangulation of $\mathbb{S}^1 \times \mathbb{S}^1$.  The simplicial complex that we construct
	      is the triangulation of the torus given in Figure 6.4 of Armstrong's book
	      {\it Basic Topology} and has 18 facets.
	 Example   
	      S = ZZ/101[a00,a10,a20,a01,a11,a21,a02,a12,a22];
	      --S = ZZ[a00,a10,a20,a01,a11,a21,a02,a12,a22]; for some reason get an error 
	      -- if use ZZ coefs...
	      -- there will be 18 facets of SS^1 x SS^1
	      Delta = simplicialComplex {a00*a02*a10, a02*a12*a10, a01*a02*a12, a01*a11*a12, a00*a01*a11, a00*a10*a11, a12*a10*a20, a12*a20*a22, a11*a12*a22, a11*a22*a21, a10*a11*a21, a10*a21*a20, a20*a22*a00, a22*a00*a02, a21*a22*a02, a21*a02*a01, a20*a21*a01, a20*a01*a00}
	 Text
	      We can check that the homology of the simplicial complex
	      $\Delta$ agrees with that of the torus
	      $\mathbb{S}^1 \times \mathbb{S}^1 $
	 Example          
	      C = truncate(chainComplex complex Delta,1)
	      prune HH C
	 Text
	      Let $S$ be the simplicial complex with facets $\{A_0 A_1, A_0 A_2, A_1 A_2\}$.  Then $S$ is a triangulation of $S^1$.  The simplicial map
	      $\pi : \Delta \rightarrow S$ given by $\pi(a_{i,j}) = A_i$ is a combinatorial realization of the trivial fibration
	      $\mathbb{S}^1 \rightarrow \mathbb{S}^1 \times \mathbb{S}^1 \rightarrow \mathbb{S}^1$.
	      We now make subsimplicial complexes arising from the filtrations of the
	      inverse images of the simplicies.
	 Example         
	      F1Delta = Delta;
	      F0Delta = simplicialComplex {a00*a01, a01*a02, a00*a02, a10*a11,a11*a12,a10*a12, a21*a20,a21*a22,a20*a22};
	      K = filteredComplex({F1Delta, F0Delta}, ReducedHomology => false) ;
	 Text
	      The resulting spectral sequence is:    
	 Example    
	      E = prune spectralSequence K
	      E^0
	      E^0 .dd
	      E^1 	      
	      E^1 .dd
	      E^2
///

doc ///
    Key
      "Spectral sequences and non-Koszul syzygies"
    Description
    	  Text
	       We illustrate some aspects of the paper 
	       "A case study in bigraded commutative algebra" by Cox-Dickenstein-Schenck.
	       In that paper, an appropriate term on the E_2 page of a suitable 
	       spectral sequence corresponds to non-koszul syzygies.
	       
	       Using our indexing conventions, the E^2_{3,-1} term will be what the
	       $E^{0,1}_2$ term is in their paper.
	       
	       We illustrate an instance of the non-generic case for non-Koszul syzygies.
	       To do this we look at the three polynomials used in their Example 4.3.
    	       The behaviour that we expect to exhibit is predicted by their Proposition 5.2.
	  Example
		R = QQ[x,y,z,w, Degrees => {{1,0},{1,0},{0,1},{0,1}}];
		B = ideal(x*z, x*w, y*z, y*w);
		p_0 = x^2*z;
		p_1 = y^2*w;
		p_2 = y^2*z+x^2*w;
		I = ideal(p_0,p_1,p_2);
		-- make the frobenious power of the irrelevant ideal
		B = B_*/(x -> x^2)//ideal;
		-- need to take a large enough power. 
		-- it turns out that 2 is large enough for this example 
		G = complete res image gens B;
		F = koszul gens I;
		K = Hom(G, filteredComplex(F));
		E = prune spectralSequence K;
		E^1
		E^2
	  Text
	        The degree zero piece of the module $E^2_{3,-1}$ twisted by $R((2,3))$ below
		shows that there is a $1$-dimensional space of non-Koszul syzygies
		of bi-degree $(2,3)$.  This is what is predicted by the paper.
    	  Example		
		E^2_{3,-1}
		basis({0,0}, E^2_{3, -1} ** R^{{2, 3}})
		E^2 .dd_{3, -1}
--		E^2 .dd
		basis({0,0}, image E^2 .dd_{3,-1} ** R^{{2,3}})
		basis({0,0}, E^2_{1,0} ** R^{{2,3}})
		-- this shows that there is a 1 dimensional space of non-Koszul syzygies of bi-degree (2,3)
		-- which is also what is predicted by the paper.
    	  Text
	      	The degree zero piece of the module $E^2_{3,-1}$ twisted by $R((6,1))$ below
		shows that there is a $1$-dimensional space of non-Koszul syzygies of bi-degree
		$(6,1)$.  This is also what is predicted by the paper.
	  Example			
		basis({0,0}, E^2 _{3, -1} ** R^{{6,1}})
		-- this shows that there is a 1 dimensional space of non-Koszul syzygies of bi-degree (6,1)
		-- this is what is predicted by the paper.
		isIsomorphism(E^2 .dd_{3, -1})	      
///	  
     
doc ///
     Key
       "Spectral sequences and connecting morphisms"
     Description
     	  Text
	       If $0 \rightarrow A \rightarrow B \rightarrow C \rightarrow 0$ is a 
	       short exact sequence of chain complexes then the connecting morphism
	       $H_i(C) \rightarrow H_{i - 1}(A)$ can realized as a suitable map
	       on the $E^1$ of a spectral sequence determined by a suitably defined
	       two step filtration of $B$.
	       
	       Here we illustrate this realization in a concrete situation:  we
	       compute the connecting morphism $H^i(X, F) \rightarrow H^{i + 1}(X, G)$
	       arising from a short exact sequence 
	       $0 \rightarrow G \rightarrow H \rightarrow F \rightarrow 0$ of sheaves
	       on a smooth toric variety $X$.
	       
 	       More specifically we let $X = \mathbb{P}^1 \times \mathbb{P}^1$ and use multigraded commutative algebra
	       together with spectral sequences to compute the connecting
	       morphism $H^1(C, OO_C(1,0)) \rightarrow H^2(X, OO_X(-2,-3))$ where 
	       $C$ is a general divisor of type $(3,3)$ on $X$.  This connecting morphism is an
	       isomorphism. 
	  Example   
                R = ZZ/101[a_0..b_1, Degrees=>{2:{1,0},2:{0,1}}]; -- PP^1 x PP^1
		M = intersect(ideal(a_0,a_1),ideal(b_0,b_1)) ; -- irrelevant ideal
		M = M_*/(x -> x^5)//ideal ; -- Suitably high Frobenius power of M
		G = res image gens M ;
		I = ideal random(R^1, R^{{-3,-3}}) -- ideal of C
	        b = chainComplex gradedModule R^{{1,0}} -- make line bundle a chain complex
		a = chainComplex gradedModule R^{{-2,-3}}
		-- make the map OO(-2, -3) --> OO(1,0)     
		f = chainComplexMap(b, a,{random(R^1, R^{{-3,-3}})}) ; 
		K = filteredComplex ({Hom(G,f)}) ; -- the two step filtered complex we want
		E = prune spectralSequence K ;
    	  Text
	       The degree zero piece of the map $E^1 .dd_{1, -2}$ below is the desired connecting 
	       morphism $H^1(C, OO_C(1,0)) \rightarrow H^2(X, OO_X(-2,-3))$.
	  Example     
		E^1 .dd_{1,-2} -- the connecting map HH^1(C, OO_C(1,0)) --> HH^2(X, OO_X(-2,-3)) 
		basis({0,0}, image E^1 .dd_{1,-2})  -- image 2-dimensional
		basis({0,0}, ker E^1 .dd_{1,-2}) -- map is injective
		basis({0,0}, target E^1 .dd_{1,-2}) -- target 2-dimensional 
		basis({0,0}, source E^1 .dd_{1,-2}) -- source 2 dimensional 
	  Text
	       An alternative way to compute the connecting morphism is 
	  Example
	      	prune connectingMorphism(Hom(G, f), - 2) ;
		prune connectingMorphism(Hom(G, f), - 2) == E^1 .dd_{1, -2} 
     
///     
     
doc ///
     Key
       "Spectral sequences and hypercohomology calculations"
   --  Headline
     --	  using spectral sequences to compute hypercohomology
     Description
     	  Text
	       If $\mathcal{F}$ is a coherent sheaf on a smooth toric variety $X$
	       then multigraded commutative algebra can be used to compute
	       the cohomology groups $H^i(X, \mathcal{F})$.  
	       
	       Indeed if $B$ is the irrelevant ideal of $X$ then the cohomology group
	       $H^i(X, \mathcal{F})$ can be realized as the degree zero piece of the multigraded
	       module
	       $Ext^i(B^{[l]}, F)$ for sufficiently large $l$; here $B^{[l]}$ denotes
	       the $l$th Frobenius power of $B$ and $F$ is any multigraded module whose
	       corresponding sheaf on $X$ is $\mathcal{F}$.  
	       
	       Given the fan of
	       $X$ and $F$ a sufficiently large power of $l$ can be determined effectively.
	       We refer to sections 2 and 3 of the paper 
	       "Cohomology on Toric Varieties and Local Cohomology with Monomial Supports"
	       for more details.
	       
	       In this example, we consider
	       the case that $X = \mathbb{P}^1 \times \mathbb{P}^1$ and 
	       $F = \mathcal{O}_C(1,0)$ where 
	       $C$ is a general divisor of type $(3,3)$ on $X$. 
	       In this setting, $H^0(C,F)$ and $H^1(C, F)$ are both $2$-dimensional 
	       vector spaces.
	       
	       We first make the multi-graded coordinate ring of 
	       $\mathbb{P}^1 \times \mathbb{P}^1$, the 
	       irrelevant ideal, and a sufficentily high Frobenus power of the 
	       irrelevant ideal needed for our calculations.  Also the complex $G$
	       below is a resolution of the irrelevant ideal.
    	  Example
	         -- C \subseteq PP^1 x PP^1 type (3,3)
		 -- Use hypercohomology to compute HH OO_C(1,0) 
		 R = ZZ/101[a_0..b_1, Degrees=>{2:{1,0},2:{0,1}}]; -- PP^1 x PP^1
		 B = intersect(ideal(a_0,a_1),ideal(b_0,b_1)) ; -- irrelevant ideal
		 B = B_*/(x -> x^5)//ideal ; -- Sufficentily high Frobenius power 
		 G = res image gens B ;
	  Text
	       We next make the ideal, denoted by $I$ below, of a general divisor of type $(3,3)$ 
	       on $\mathbb{P}^1 \times \mathbb{P}^1$.  Also the chain complex
	       $F$ below is a resolution of this ideal.
	  Example
	  	 I = ideal random(R^1, R^{{-3,-3}}) ; -- ideal of C
		 F = res comodule I 
    	  Text
	       To use hypercohomology to compute the cohomology groups of the 
	       line bundle $\mathcal{O}_C(1,0)$ on $C$ we twist the
	       complex $F$ above by a line of ruling and then 
	       make a filtered complex whose associated spectral
	       sequence abuts to the desired cohomology groups.
	  Example     		 		 
		 K = Hom(G , filteredComplex (F ** R^{{1,0}})) ; -- Twist F by a line of ruling and make filtered complex whose ss abuts to HH OO_C(1,0) 
		 E = prune spectralSequence K ; --the spectral sequence degenerates on the second page 
		 E^1 
		 E^2 ; -- output is a mess
    	  Text 
	       The cohomology groups we want are obtained as follows.
	  Example     		 
		 basis({0,0}, E^2_{0,0}) --  == HH^0 OO_C(1,0)
		 basis({0,0}, E^2_{1,-2}) --  == HH^1 OO_C(1,0)	 
		 
     SeeAlso
    	  "Spectral sequences and connecting morphisms"
    	  "Spectral sequences and non-Koszul syzygies"	 
///	  


doc ///
          Key
       	    "Computing the Serre Spectral Sequence associated to a Hopf Fibration"
	  Description
	       Text
	       	    We compute the Serre Spectral Sequence
		    associated to the Hopf Fibration 
		    $S^1 \rightarrow S^3 \rightarrow S^2$.
		    This example is made possible by the minimal
		    triangulation of this fibration given in the paper
		    "A minimal triangulation of the Hopf map and its application"
		    by K.V. Madahar and K.S Sarkaria. Geom Dedicata, 2000.
     	       Text
	       	    We first make the relevant simplicial complexes
		    described on page 110 of the paper.  The
		    simplicial complex $S3$ below is a triangulation of 
		    $S^3$.  
	       Example		    
		    B = QQ[a_0..a_2,b_0..b_2,c_0..c_2,d_0..d_2];
		    l1 = {a_0*b_0*b_1*c_1,a_0*b_0*c_0*c_1,a_0*a_1*b_1*c_1,b_0*b_1*c_1*d_1,b_0*c_0*c_1*d_2,a_0*a_1*c_1*d_2,a_0*c_0*c_1*d_2,b_0*c_1*d_1*d_2};
		    l2 = {b_1*c_1*c_2*a_2,b_1*c_1*a_1*a_2,b_1*b_2*c_2*a_2,c_1*c_2*a_2*d_1,c_1*a_1*a_2*d_2,b_1*b_2*a_2*d_2,b_1*a_1*a_2*d_2,c_1*a_2*d_1*d_2};
		    l3 = {c_2*a_2*a_0*b_0,c_2*a_2*b_2*b_0,c_2*c_0*a_0*b_0,a_2*a_0*b_0*d_1,a_2*b_2*b_0*d_2,c_2*c_0*b_0*d_2,c_2*b_2*b_0*d_2,a_2*b_0*d_1*d_2};
		    l4 = {a_0*b_0*b_1*d_1,a_0*b_1*d_0*d_1,b_1*c_1*c_2*d_1,b_1*c_2*d_0*d_1,a_0*a_2*c_2*d_1,a_0*c_2*d_0*d_1};
		    l5 = {a_0*b_1*d_0*d_2,a_0*a_1*b_1*d_2,b_1*c_2*d_0*d_2,b_1*b_2*c_2*d_2,a_0*c_2*d_0*d_2,a_0*c_0*c_2*d_2};
		    S3 = simplicialComplex(join(l1,l2,l3,l4,l5));
	       Text 
	            We identify the two sphere $S^2$ with the simplicial complex $S2$ defined
		    by the facets $\{abc, abd, bcd, acd \}$.  The Hopf fibration 
		    $S^1 \rightarrow S^3 \rightarrow S^2$ is then realized by the simplicial
		    map $p: S3 \rightarrow S2$ defined by $a_i \mapsto a$, $b_i \mapsto b$, 
		    $c_i \mapsto c$, and $d_i \mapsto d$.  
		    		    
		    We now explain how to construct the filtration of $S3$ obtained by
		    considering the $k$-skeletons of this fibration.
		    
		    The simplicial complex $F1S3$ below
		    is the subsimplicial complex of $S3$ obtained by considering the 
		    inverse images of the
		    $1$-dimensional faces of the simplicial complex $S2$. 
		    We first describe the simplicial complex $F1S3$ in pieces.		    
		    
		    For example, to compute $f1l1$ below, we observe that 
		    the inverse image of $ab$ under $p$ is
		    $a_0b_0b_1, a_0a_1b_1$ etc.
		    All of these inverse images have been computed by hand previously. 
	       Example
	            f1l1 = {a_0*b_0*b_1,a_0*a_1*b_1,a_0*c_0*c_1,a_0*a_1*c_1,a_0*a_1*d_2,d_1*d_2,b_0*b_1*c_1,b_0*c_0*c_1,b_0*b_1*d_1,b_0*d_1*d_2,c_1*d_1*d_2,c_0*c_1*d_2};
		    f1l2 = {b_1*a_1*a_2,b_1*b_2*a_2,c_1*c_2*a_2,c_1*a_1*a_2,a_1*a_2*d_2,a_2*d_1*d_2,b_1*c_1*c_2,b_1*b_2*c_2,b_1*b_2*d_2,d_1*d_2,c_1*d_1*d_2,c_1*c_2*d_1};
		    f1l3 = {a_2*a_0*b_0,a_2*b_2*b_0, c_2*a_2*a_0,c_2*c_0*a_0,a_2*a_0*d_1,a_2*d_1*d_2,b_2*b_0*c_2,c_2*c_0*b_0,b_2*b_0*d_2,b_0*d_1*d_2,c_2*c_0*d_2,d_1*d_2};
		    f1l4 = {a_0*b_0*b_1,a_0*a_2,a_0*a_2*c_2,c_1*c_2,a_0*d_0*d_1,a_0*a_2*d_1,b_1*c_1*c_2,b_0*b_1,b_0*b_1*d_1,b_1*d_0*d_1,c_1*c_2*d_1,c_2*d_0*d_1}
		    f1l5 = {a_0*a_1*b_1,b_1*b_2,a_0*c_0*c_2,a_0*a_1,a_0*d_0*d_2,a_0*a_1*d_2,b_1*b_2*c_2,c_0*c_2,b_1*d_0*d_2,b_1*b_2*d_2,c_2*d_0*d_2,c_0*c_2*d_2};
		    F1S3 = simplicialComplex(join(f1l1,f1l2,f1l3,f1l4,f1l5));
	       Text
	            The simplicial complex $F0S3$ below is the subsimplicial complex of $F1S3$ 
		    obtained by considering the inverse images of
		    the $0$-dimensional faces of the simplicial complex $S2$.  Again we describe 
		    this simplicial complex in pieces.
	       Example
		    f0l1 = {a_0*a_1,b_0*b_1,c_0*c_1,d_1*d_2};
		    f0l2 = {a_1*a_2,b_1*b_2,c_1*c_2,d_1*d_2};
		    f0l3 = {a_0*a_2,b_0*b_2,c_0*c_2,d_1*d_2};
		    f0l4 = {a_0*a_2,b_0*b_1,c_1*c_2,d_0*d_1};
		    f0l5 = {a_0*a_1,b_1*b_2,c_0*c_2,d_0*d_2};
		    F0S3 = simplicialComplex(join(f0l1,f0l2,f0l3,f0l4,f0l5)); 
	       Text
	            The simplicial complex $S3$ is obtained by considering the 
		    inverse images of the $2$ dimensional faces of $S2$.
		    
		    To compute a simplicial version of
		    the Serre spectral sequence for the
		    $S^1 \rightarrow S^3 \rightarrow S^2$ 
		    correctly, meaning that the spectral sequence takes the form
		    $E^2_{p,q} = H_p(S^2,H_q(S^1,QQ))$, we need to
		    use non-reduced homology.		     
     	       Example		     
     	       	    K = filteredComplex({S3,F1S3,F0S3}, ReducedHomology => false);
     	       Text		    
		    We now compute the various pages of the spectral sequence.
		    To make the output 
		    intelligible we prune the spectral sequence.
     	       Example		     
     	       	    E = prune spectralSequence K;
     	       Example		    		    
		    E0 = E^0
     	       Text
	       	    Here are the maps.
     	       Example		    		    
     	       	    E0.dd
     	       Text
	       	    Now try the $E^1$ page.  
     	       Example		       	       	    
		    E1 = E^1
     	       Text 
	       	    Here are the maps.
     	       Example		    		    
		    E1.dd 
     	       Text
	       	    Now try the $E^2$ page.
     	       Example		    		    
		    E2 = E^2
     	       Text
	       	    Here are the maps.
     	       Example		    		    
		    E2.dd
     	       Text		    
		    Note that the modules on the $E^2$ page appear to have been computed correctly.  
		    The statement of the Serre spectral sequence, see for example Theorem 1.3 p. 8 of 
		    Hatcher's Spectral Sequence book, asserts that 
	            $E^2_{p,q} = H_p(S^2,H_q(S^1,QQ))$.
		    This is exactly what we obtained above.  Also 
		    the maps on the $E^2$ page also seem to be computed correctly as the spectral sequence
		    will abut to the homology of $S^3$.
     	       Example		    
		    E3 = E^3
		    E3.dd
     	       Text		    
		   Thus the E^3 page appears to have been computed correctly.		
///	       
 
doc ///
      Key
      	   "Balancing Tor"
     Description
     	  Text
	       To balance Tor we first need to make some modules over a ring.
     	  Example
	       A = QQ[x,y,z,w];
	       M = monomialCurveIdeal(A,{1,2,3});
	       N = monomialCurveIdeal(A,{1,3,4});
     	  Text
	       To compute $Tor^A_i(M,N)$ we resolve the modules, tensor appropriately, 
	       and then take homology. 
     	  Example	       	       	       
	       K = res M
	       J = res N
     	  Text
	       The spectral sequence that computes $Tor^A_i(M,N)$ by tensoring
	       $K$ with $N$ and taking homology is given by
     	  Example
	       E = prune spectralSequence((filteredComplex K) ** J)
     	  Text
	       The spectral sequence that computes $Tor^A_i(M,N)$ by tensoring
	       $J$ with $M$ and taking homology is given by 	        
     	  Example
	       F = prune spectralSequence((K ** (filteredComplex J)))	       
	  Text
	       Let's compute some pages and maps of these spectral sequences.
	       The zeroth pages takes the form:
	  Example
	       E^0
	       E^0 .dd
	       F^0 
--	       F^0 .dd
	  Text
	      The first pages take the form:
	  Example          
	       E^1
--	       E^1 .dd
    	       F^1
--	       F^1 .dd
	 Text 
	       The second pages take the form:      
         Example
     	       E^2
--	       E^2 .dd
    	       F^2
--	       F^2 .dd
	 Text
	       Observe that $E^2$ and $F^2$ are equal as they should.	 
     SeeAlso
	    "Filtrations and tensor product complexes"
	    "Filtrations and homomorphism complexes"	          
///	       	 


doc ///
     Key
     	  "Examples of change of rings Spectral Sequences"
     Description
     	  Text
	       Here are some examples of change of rings spectral sequences. 
	  Text
	       Given a ring map f: R -> S, an R-module M and an R-module S,
	       there is a spectral sequence E with E^2_{p,q} = Tor^S_p(Tor^R_q(M,S),N)
	       that abuts to Tor^R_{p+q}(M,N).
     	  Example
--	       First example
	       k=QQ;
	       R=k[a,b,c];
	       S=k[s,t];
	       f = map(S,R,{s^2,s*t,t^2});
	       N = coker vars S;
	       M = coker vars R --;
	       F := complete res N;
	       pushFwdF := pushFwd(f,F);
	       G := complete res M;
	       E := spectralSequence(filteredComplex(G) ** pushFwdF);
	       EE := spectralSequence(G ** (filteredComplex pushFwdF));
     	       e = prune E;
	       ee = prune EE;
	       e^0
	       e^1
	       e^2
	       e^infinity
	       ee^0
     SeeAlso
	    "Filtrations and tensor product complexes"	    
 	  	  
///

--------------------------------------------
-- Documentation of methods and functions --
--------------------------------------------

--
-- Types
--

doc ///
     Key
     	  FilteredComplex
     Headline
     	  the type of all filtered complexes
     Description
     	  Text	 
	     An ascending filtration of a bounded (homological, lower index, or degree $-1$) chain complex
	     $C : \dots \rightarrow C_i \rightarrow C_{i - 1} \rightarrow \dots$
	     is an ordered family of chain subcomplexes 
	     $FC : \dots \subseteq F_{n - 1} C \subseteq F_n C \subseteq \dots $.
	     Such a filtration is said to be bounded if $F_s C = C$ for all sufficiently
	     large $s$ and $F_t C = 0$ for all sufficiently small $t$.
	     
	     Alternatively, a descending filtration of a bounded (cohomological, or upper index, or degree $1$) chain complex 
	     $C : \dots  \rightarrow C^i \rightarrow C^{i + 1} \rightarrow \dots $
	     is an ordered family of subchain complexes 
	     $FC : \dots \subseteq F^{n + 1} C \subseteq F^n C \subseteq \dots$.
	     Such a filtration is said to be bounded if $F^s C = 0$ for all sufficiently 
	     large $s$ and $F^t C = C$ for all sufficiently small $t$.
	      
	     The type {\tt FilteredComplex} is a data type for working with bounded filtrations of bounded chain complexes.
     Caveat
     	  By assumption all filtered complexes arise from bounded filtrations of bounded chain complexes.  Filtrations on degree $-1$
	  chain complexes are ascending.  Filtrations on degree $1$ chain complexes are
	  descending.
    SeeAlso
    	  "How to make filtered complexes from chain complex maps"
	  "Filtered complexes and simplicial complexes"
	  "Filtrations and tensor product complexes"
	  "Filtrations and homomorphism complexes"	  
///

doc ///
     Key
     	  SpectralSequence
     Headline
     	  the type of all spectral sequences
     Description
     	  Text
	       A (homological, or lower index) spectral sequence consists of:
	       	      
	       1. A sequence of modules $\{E^r_{p,q}\}$ for $p,q \in \mathbb{Z}$ and $r \geq 0$;
	       
       	       2. A collection of homomorphisms $\{d^r_{p,q}: E^r_{p,q} \rightarrow E^r_{p-r,q+r-1} \}$, for $p,q \in \mathbb{Z}$ and $ r \geq 0$, such that
	       $d^r_{p,q} d^r_{p+r,q-r+1} = 0$ ; 	       
	       
	       3. A collection of isomorphisms $E^{r+1}_{p,q}  \rightarrow  ker d^r_{p,q} / image d^r_{p+r,q-r+1}$.	       
	       
	       Alternatively a (cohomological, or upper index) spectral sequence consists of:
	       
	       1'. A sequence of modules $\{E_r^{p,q}\}$ for $p,q \in \mathbb{Z}$, and $r \geq 0$;
	       
       	       2'. A collection of homomorphisms $\{d_r^{p,q}: E_r^{p,q} \rightarrow E_{r}^{p+r,q-r+1}\}$ for $p,q \in \mathbb{Z}, r \geq 0$ such that
	       $d_r^{p,q} d_r^{p-r,q+r-1} = 0$ ; 	       
	       
	       3'. A collection of isomorphisms $E_{r+1}^{p,q}  $\rightarrow$ ker d_r^{p,q} / image d_r^{p-r,q+r-1}$.	       
	       
	       The type {\tt SpectralSequence} is a data type for working with spectral sequences. 
	       In this package, a spectral sequence is represented by a sequence of spectral sequence pages.
     Caveat
     	  All spectral sequences arise from bounded filtrations of bounded chain complexes.  Ascending filtrations of degree $-1$ chain complexes
	  determine spectral sequences of the first type.  Descending filtrations of degree $1$ chain complex determine spectral sequences of the second type.
     SeeAlso
     	 "SpectralSequencePage"
	 "SpectralSequencePageMap"
     	 "Filtered complexes and simplicial complexes"
	 "Filtrations and tensor product complexes"
	 "Filtrations and homomorphism complexes"
///



doc ///
          Key
       	   (describe, SpectralSequence)
          Headline
	       real description
     	  Usage
	       describe S
	  Description
	       Text
	       	   see describe
    	  SeeAlso
	      	describe	      
///



doc ///
     Key
     	  SpectralSequencePage
     Headline
     	  the type of all spectral sequence pages
     Description
     	  Text
	       A (homological, or lower index) spectral sequence page consists of:
	       
	       1. A fixed integer $r \geq 0$, the page number;	      
	       
	       2. A sequence of modules $\{E^r_{p,q}\}$ for $p,q \in \mathbb{Z}$;
	       
       	       3. A collection of homomorphisms $\{d^r_{p,q}: E^r_{p,q} \rightarrow E^r_{p-r,q+r-1}\}$ for
	       $p,q \in \mathbb{Z}, r \geq 0$ such that
	       $d^r_{p,q} d^r_{p+r,q-r+1} = 0$ ; 	       
	       
	       4. A collection of isomorphisms $E^{r+1}_{p,q}  \rightarrow ker d^r_{p,q} / image d^r_{p+r,q-r+1}$.	       
	       
	       Alternatively a (cohomological, or upper index) spectral sequence page consists of:
	       
	       1'.  A fixed integer $r \geq 0$, the page number;
	       
	       2'. A sequence of modules $\{E_r^{p,q}\}$ for $p,q \in \mathbb{Z}$;
	       
       	       3'. A collection of homomorphisms $\{d_r^{p,q}: E_r^{p,q} \rightarrow E_r^{p+r,q-r+1}\}$ for
	       $ p,q \in \mathbb{Z}, r \geq 0$ such that
	       $d_r^{p,q} d_r^{p-r,q+r-1} = 0$ ; 	       
	       
	       4'. A collection of isomorphisms $E_{r+1}^{p,q}  \rightarrow ker d_r^{p,q} / image d_r^{p-r,q+r-1}$.	
	       
	       The type {\tt SpectralSequencePage} is a data type for working with spectral sequences
	       and spectral sequence pages.
     Caveat
	    The isomorphisms $4$ and $4$' are not explicitly
	    part of the data type, although they can be obtained by using the command @TO"homologyIsomorphism"@.
    SeeAlso
    	"SpectralSequence"
	"SpectralSequencePageMap"
	"Page"	            	    	      
///	       

doc ///
          Key
           (support, FilteredComplex)
///


doc ///
          Key
           (expression, SpectralSequence)
///


doc ///
          Key
           (support, PageMap)
///


doc ///
          Key
           (support, SpectralSequencePage)
///


doc ///
          Key
           (spots, PageMap)
///


doc ///
          Key
	   spectralSequencePageMap
           (spectralSequencePageMap, FilteredComplex, ZZ)
     Headline 
	  compute the maps on a spectral sequence page 
     Usage 
         d = spectralSequencePageMap(FilteredComplex, ZZ)
     Inputs
	 K:FilteredComplex
	 n:ZZ	 
     Outputs
         D:SpectralSequencePageMap 
     Description
	  Text
	       Returns the differentials of a spectral sequence page.
///



doc ///
          Key
           (ring, Page)
///


doc ///
          Key
           (symbol _, PageMap, List)
///


doc ///
          Key
           (page, SpectralSequencePage)
///


doc ///
          Key
           (symbol _, Page, List)
///



doc ///
          Key
           (net, SpectralSequence)
///


doc ///
          Key
           (net, SpectralSequencePage)
///


doc ///
          Key
           (net, PageMap)
///

doc ///
          Key
           (net, Page)
///


doc ///
          Key
           (net, FilteredComplex)
///


doc ///
          Key
           (degree, Page)
///

--doc ///
--          Key
--           spectralSequencePageMap
--///

doc ///
          Key
           pageMap
///

doc ///
          Key
	   page
           (page,List,List,Page)
     	  Description
	       Text
	           adds keys to a a page.
          SeeAlso
                Page       	   
///


--doc ///
--          Key
--           page
--///


doc ///
          Key
       	   (describe, SpectralSequencePage)
          Headline
	       real description
     	  Usage
	       describe S
	  Description
	       Text
	       	   see describe
    	  SeeAlso
	      	describe	      
///




doc ///
     Key
     	  SpectralSequencePageMap
     Headline
     	  the type of all spectral sequence page maps
     Description
     	  Text
	       A (homological, or lower index) spectral sequence page map consists of:
	       
	       1.  A fixed integer $r \geq 0 $, the page number;
	       
	       2. A collection of homomorphisms $\{d^r_{p,q}: E^r_{p,q} \rightarrow E^r_{p-r,q+r-1}\}$ for $p,q \in \mathbb{Z}, r \geq 0$ such that
	       $d^r_{p,q} d^r_{p+r,q-r+1} = 0$. 
	       
	       Alternatively a (cohomological, or upper index) spectral sequence page consists of:
	       
	       1'.  A fixed integer $r \geq 0$, the page number;
	       
	       2'.   A collection of homomorphisms $\{d_r^{p,q}: E_r^{p,q} \rightarrow E_r^{p+r,q-r+1}\}$ for $p,q \in \mathbb{Z}, r \geq 0$ such that
	       $d_r^{p,q} d_r^{p-r,q+r-1} = 0$.
	       
	       The type {\tt SpectralSequencePageMap} is a data type for working with spectral sequences and the differentials
	       on the pages of a spectral sequence.
    SeeAlso
    	"SpectralSequence"
	"SpectralSequencePage"
	"PageMap"	       
///	       



doc ///
          Key
       	   (describe, SpectralSequencePageMap)
          Headline
	       real description
     	  Usage
	       describe S
	  Description
	       Text
	       	   see describe
    	  SeeAlso
	      	describe	      
///


doc ///
     Key
     	  Page
     Headline
     	  the type of all pages
     Description
     	  Text
	       A page is a collection of modules which are indexed by lists of integers.  This is a parent class for the type @TO"SpectralSequencePage"@.  The infinity page of a spectral sequence 
	       is an example of a page which is not a spectral sequence page.
	       
	      As a specific example consider the filtered complex $K$ below, obtained by multiplying the minimal free resolution of
	      the rational quartic space curve by successive powers of the irrelevant ideal.
	 Example     
	      B = QQ[a..d];
	      J = ideal vars B;
	      C = complete res monomialCurveIdeal(B,{1,3,4});
	      K = filteredComplex(J,C,4);
	 Text
	      The infinity page of the resulting spectral sequence is computed below.
	 Example
	       E = prune spectralSequence K;
	       E^infinity
     SeeAlso
       SpectralSequencePage
       (symbol ^, SpectralSequence, InfiniteNumber)
       (symbol _, SpectralSequence, InfiniteNumber)	       
///	       



doc ///
          Key
       	   (describe, Page)
          Headline
	       real description
     	  Usage
	       describe S
	  Description
	       Text
	       	   see describe
    	  SeeAlso
	      	describe	      
///

doc ///
          Key
       	   (NewMethod, Page)
///



doc ///
          Key
	   spots
       	   (spots, Page)
          Headline
	       which spots does the given page has a module.
     	  Usage
	       s = spots P
	  Inputs
	       P:Page	  
	  Outputs
	       s:List 
	  Description
	       Text
	       	   Returns a list of all the spots where the given page has a module.
    	  SeeAlso
	      		      
///


doc ///
          Key
       	   (support, Page)
          Headline
	       which non-zero modules appear in the given page.
     	  Usage
	       l = support P
	  Inputs
	       P:Page	  
	  Outputs
	       l:List 
	  Description
	       Text
	       	   Returns a list of all the non-zero modules appearing in the given page has a module.
    	  SeeAlso
	      		      
///


doc ///
     Key
     	  PageMap
     Headline
     	  the type of all page maps
     Description
     	  Text
	       A page map is a collection of homomorphisms which are indexed by lists of integers.  This is a parent class for the type @TO"SpectralSequencePageMap"@.  The output of the 
	       method {\tt pruningMaps(SpectralSequencePage)} is an example of a {\tt Page} which is not a {\tt SpectralSequencePage}.
	       
	       As a specific example consider the filtered complex $K$ below, obtained by multiplying the minimal free resolution of
	       the rational quartic space curve by successive powers of the irrelevant ideal.
	  Example     
	       B = QQ[a..d];
	       J = ideal vars B;
	       C = complete res monomialCurveIdeal(B,{1,3,4});
	       K = filteredComplex(J,C,4);
	  Text
	       We compute an example of a pruning map below.
	  Example
	       E = prune spectralSequence K;
	       pruningMaps E^2
     SeeAlso
     	 (pruningMaps,SpectralSequencePage)  
	 prune  
///	       

doc ///
          Key
       	   (describe, PageMap)
          Headline
	       real description
     	  Usage
	       describe S
	  Description
	       Text
	       	   see describe
    	  SeeAlso
	      	describe	      
///

--- functions and methods --- 

doc ///
          Key
       	   (spots, ChainComplex)
          Headline
	       which spots does the given chain complex has a module.
     	  Usage
	       s = spots L
	  Inputs
	       L:ChainComplex   	  
	  Outputs
	       s:List 
	  Description
	       Text
	       	   Returns a list of all the spots where the given chain complex has a module.
    	  SeeAlso
	      		      
///


doc ///
          Key
       	    filteredComplex
          Headline
	       make a filtered complex
     	  Usage
	       K = filteredComplex L
	  Inputs
	       L:List       	  
		    	 or 
	       L:ChainComplex 
	       	    	 or
     	       L:SpectralSequence			 
	       ReducedHomology => Boolean	       	  	    
	       Shift => ZZ
	  Outputs 
	       K: FilteredComplex
	  Description
	       Text
	       	    This is the primitive filtered complex constructor.   
    	  SeeAlso
	      FilteredComplex
	      "How to make filtered complexes from chain complex maps"
    	      "Filtrations and tensor product complexes"
    	      "Filtrations and homomorphism complexes"
    	      "Filtered complexes and simplicial complexes"		      
///


doc ///
          Key
       	   (spots, FilteredComplex)
          Headline
	       which spots does the given filtered complex has a module.
     	  Usage
	       s = spots L
	  Inputs
	       L:FilteredComplex   	  
	  Outputs
	       s:List
	  Description
	       Text
	       	   Returns a list of all the spots where the given filtered complex has a module.
    	  SeeAlso
	      		      
///

doc ///
          Key
       	   (max, FilteredComplex)
          Headline
	       maximum spot where the given filtered complex has a module.
     	  Usage
	       m = max L
	  Inputs
	       L:FilteredComplex   	  
	  Outputs
	       m:ZZ
	  Description
	       Text
	       	   Returns the maximum spot where the given filtered complex has a module.
    	  SeeAlso
	      		      
///



doc ///
          Key
       	   (min, FilteredComplex)
          Headline
	       minimum spot where the given filtered complex has a module.
     	  Usage
	       m = min L
	  Inputs
	       L:FilteredComplex   	  
	  Outputs
	       m:ZZ
	  Description
	       Text
	       	   Returns the minimum spot where the given filtered complex has a module.
    	  SeeAlso
	      		      
///

doc ///
     Key
       spectralSequence
     Headline
     	  construct a spectral sequence
     Usage
     	  E = spectralSequence K
     Inputs
     	  K:FilteredComplex
	       A filtered complex
     Outputs
     	  E:SpectralSequence
     Description
     	  Text 
	       This is the primitive spectral sequence constructor.
	       
	       In the example below we construct a spectral sequence
	       $E$ from the filtered complex $K$.
	  Example     
	       B = QQ[a..d];
	       J = ideal vars B;
	       C = complete res monomialCurveIdeal(B,{1,3,4});
	       K = filteredComplex(J,C,4);
	       E = spectralSequence K
	  Text
	       To view pages and or maps we proceed, for example, as follows (note we suppress the output of the E^0.dd command to prevent excessive output)
	  Example
	       E^0
	       E^0 .dd;
	       E^infinity                   
     SeeAlso
     	 SpectralSequence
	 SpectralSequencePage
	 (symbol ^,SpectralSequence,ZZ)
     	 (spectralSequence, FilteredComplex) 
	 "Examples of filtered complexes and spectral sequences"   
///

doc ///
     Key
        spectralSequencePage
     Headline
     	  construct a spectral sequence page from a filtered complex
     Usage
     	  E = spectralSequencePage(K,r)
     Inputs
     	  K:FilteredComplex
	       A filtered complex
	  r:ZZ
     Outputs
     	  E:SpectralSequencePage
     Description
     	  Text 
	       This is the primitive spectral sequence page constructor.
	  Example     
	       B = QQ[a..d];
	       J = ideal vars B;
	       C = complete res monomialCurveIdeal(B,{1,3,4});
	       K = filteredComplex(J,C,4);
	       E = spectralSequence K
	  Text
	       To view pages and or maps we proceed, for example, as follows
	  Example
	       E^0
	       
     SeeAlso
     	 spectralSequence
     	 (spectralSequence, FilteredComplex)	       
	 SpectralSequencePageMap
	 "Examples of filtered complexes and spectral sequences"   
///



doc ///
    	  Key
	    (truncate, ChainComplex, ZZ)
	  Headline 
	    compute the hard truncation of a chain complex   
     Description
     	  Text
	       Computes the hard truncation of a chain complex as a specified homological degree.
	  Example
	       B = QQ[a..d];
	       C = koszul vars B
	       truncate(C,1)
	       truncate(C,-1)
	       truncate(C,-10)
	       truncate(C,10)     	    
///	       

doc ///
     Key
	  pruningMaps
     Headline 
	  compute the pruning maps on a spectral sequence page
     Usage
     	  d = pruningMaps E
     Inputs
     	  E:SpectralSequencePage
     Outputs
     	  d:PageMap
     Description
     	  Text 
	       Returns the pruning maps which are cached in the process of pruning the spectral sequence page.
	       
	       As a specific example consider the filtered complex $K$ below, obtained by multiplying the minimal free resolution of
	       the rational quartic space curve by successive powers of the irrelevant ideal.
	  Example     
	       B = QQ[a..d];
	       J = ideal vars B;
	       C = complete res monomialCurveIdeal(B,{1,3,4});
	       K = filteredComplex(J,C,4);
	  Text
	       We compute an example of a pruning map below.
	  Example
	       E = prune spectralSequence K;
	       pruningMaps E^2
     
     SeeAlso
	      (prune, SpectralSequence)
	      SpectralSequencePage
	      PageMap  
///	       

doc ///
    	  Key
	    (support,ChainComplex)
	  Headline 
	    nonzero parts of a chain complex
	  Description
	      Text
	            Computes the homological degrees in which the chain complex admits a nonzero module  
		    
	      Example
	      	    A = QQ[x,y];
		    C = koszul vars A
		    support C
		    D = truncate(C,1)
		    spots D
		    support D
    	  SeeAlso
	       (spots, ChainComplex)	     	    
///	       


doc ///
    	  Key
	    (pruningMaps, SpectralSequencePage)
	  Headline 
	    compute the pruning maps on a spectral sequence page
     Description
     	  Text 
	       Returns the pruning maps which are cached in the process of pruning the spectral sequence page.
	       
	       As a specific example consider the filtered complex $K$ below, obtained by multiplying the minimal free resolution of
	       the rational quartic space curve by successive powers of the irrelevant ideal.
	  Example     
	       B = QQ[a..d];
	       J = ideal vars B;
	       C = complete res monomialCurveIdeal(B,{1,3,4});
	       K = filteredComplex(J,C,4);
	  Text
	       We compute an example of a pruning map below.
	  Example
	       E = prune spectralSequence K;
	       pruningMaps E^2
     
     SeeAlso
	      (prune, SpectralSequence)
	      SpectralSequencePage
	      PageMap  
///	       

doc ///
     Key 
      (filteredComplex, List)
     Headline 
      obtain a filtered complex from a list of chain complex maps or a nested list of simplicial complexes
     Usage 
       K = filteredComplex L 
     Inputs 
	  L: List
	  ReducedHomology => Boolean
	  Shift => ZZ
     Outputs 
       K: FilteredComplex
     Description
      	  Text  
       	    We can make a filtered complex from a list of chain complex maps as follows.
	    We first need to load the relevant packages.
          Example
	       needsPackage "SpectralSequences"	    
     	  Text
	       We then make a chain complex.
     	  Example	       	 
	       R = QQ[x,y,z,w]
	       d2 = matrix(R,{{1},{0}})
	       d1 = matrix(R,{{0,1}})
	       C = chainComplex({d1,d2}) 
	  Text
	      We now make the modules of the another chain complex which we will label D.
	  Example      
	       D_2 = image matrix(R,{{1}})
	       D_1 = image matrix(R,{{1,0},{0,0}})
	       D_0 = image matrix(R,{{1}})
	       D = chainComplex({inducedMap(D_0,D_1,C.dd_1),inducedMap(D_1,D_2,C.dd_2)})
     	  Text
	       Now make a chain complex map.
     	  Example	       	     
	       d = chainComplexMap(C,D,apply(spots C, i-> inducedMap(C_i,D_i,id_C _i)))
	       isChainComplexMap d
	       d == chainComplexMap(C,D,{inducedMap(C_0,D_0,id_(C_0)),inducedMap(C_1,D_1,id_(C_1)),inducedMap(C_2,D_2,id_(C_2))})
     	  Text
	       We now make the modules of another chain complex which we will label E.	     
     	  Example	      
               E_2 = image matrix(R,{{0}})
	       E_1 = image matrix(R,{{1,0},{0,0}})
	       E_0 = image matrix(R,{{1}})
	       E = chainComplex({inducedMap(E_0,E_1,C.dd_1),inducedMap(E_1,E_2,C.dd_2)})
     	  Text
	       Now make a chain complex map.
     	  Example	      	       
	       e = chainComplexMap(C,E,apply(spots C, i->inducedMap(C_i,D_i, id_C _i)))
     	  Text 
	       Now make a filtered complex from a list of chain complex maps.
     	  Example	       	       
	       K = filteredComplex({d,e})
	  Text
	     We can make a filtered complex, with a specified minimum filtration degree
             from a list of ChainComplexMaps by using the Shift option.
      	  Example	       	     
	       L = filteredComplex({d,e},Shift => 1)
	       M = filteredComplex({d,e},Shift => -1)	      	    
	  Text
	    We can make a filtered complex from a nested list of simplicial 
     	    complexes as follows
     	  Example	     
	      D = simplicialComplex {x*y*z, x*y, y*z, w*z}
	      E = simplicialComplex {x*y, w}
	      F = simplicialComplex {x,w}
	      K = filteredComplex{D,E,F}
	  Text
     	     If we want the resulting complexes to correspond to the non-reduced homology
     	     of the simplicial complexes we can do the following.
     	  Example 
	     filteredComplex({D,E,F}, ReducedHomology => false)
     SeeAlso
     	  "maps between chain complexes"
///

doc ///
     Key 
          (filteredComplex, ChainComplex)
     Headline 
         obtain a filtered complex from a chain complex
     Usage 
         K = filteredComplex C 
     Inputs 
	  C: ChainComplex
-- these options don't do anything for this constructor.
	  ReducedHomology => Boolean	       	  	    
	  Shift => ZZ
     Outputs
          K: FilteredComplex
     Description	  
     	  Text
	     Produces the filtered complex obtained by successively truncating the complex.
	  Example 
	    needsPackage "SpectralSequences"
	    A = QQ[x,y]
	    C = koszul vars A
	    K = filteredComplex C
     SeeAlso 
	  (truncate, ChainComplex,ZZ)
/// 

doc ///
     Key 
          (filteredComplex, SpectralSequence)
     Headline 
         obtain the filtered complex associated to the spectral sequence
     Usage 
         K = filteredComplex E 
     Inputs 
	  E: SpectralSequence
-- these options don't do anything for this constructor.
	  ReducedHomology => Boolean	       	  	    
	  Shift => ZZ
     Outputs
          K: FilteredComplex
     Description	  
     	  Text
	     Produces the filtered complex which determined the spectral sequence.
	     Consider the spectral sequence $E$ which arises from a nested list of simplicial
	     complexes.
	  Example 
	    A = QQ[a,b,c,d];
	    D = simplicialComplex {a*d*c, a*b, a*c, b*c};
	    F2D = D;
	    F1D = simplicialComplex {a*c, d};
	    F0D = simplicialComplex {a,d};
	    K = filteredComplex {F2D, F1D, F0D};
	    E = spectralSequence(K) ;
	  Text
	    The underlying filtered chain complex 
	    can be recovered from the
	    spectral sequence by:
	  Example     
    	    C = filteredComplex E 
     SeeAlso
          --(_, FilteredComplex,InfiniteNumber)
          --(^,FilteredComplex,InfiniteNumber)
/// 


doc ///
     Key
  	  (basis, List, SpectralSequencePage)
	  (basis, ZZ, SpectralSequencePage)
     Headline
     	  generators of a particular degree
     Usage
     	  B = basis(L, E)
     Inputs
     	  L:List
	  E:SpectralSequencePage
     Outputs
     	 B:Matrix --Note!!  The output should actually be a page!!
     Description
     	  Text 
	       Returns generators for the requested (multi)degree of the spectral sequence page.  It is designed to extend
	       the function @TO"basis"@ which can be applied to modules, for instance.

      	       As a specific example consider the filtered complex $K$ below, obtained by multiplying the minimal free resolution of
	       the rational quartic space curve by successive powers of the irrelevant ideal.
	  Example     
	       B = QQ[a..d];
	       J = ideal vars B;
	       C = complete res monomialCurveIdeal(B,{1,3,4});
	       K = filteredComplex(J,C,4);
	  Text
	       We compute the degree $0$ piece of the $E^3$ page below.
	  Example
	       E = prune spectralSequence K;
	       E^3
	       basis(0,E^3)
     SeeAlso
     	 basis	       
///
  
doc ///
     Key
  	  (hilbertPolynomial, SpectralSequencePage)
     Headline
     	  the Hilbert polynomial of a spectral sequence page
     Usage
     	  H = hilbertPolynomial(E)
     Inputs
	  E:SpectralSequencePage
     Outputs
     	 H:Page
     Description
     	  Text 
	       Returns the Hilbert polynomials of all modules of the spectral sequence page
      	       
	       As a specific example consider the filtered complex $K$ below, obtained by multiplying the minimal free resolution of
	       the rational quartic space curve by successive powers of the irrelevant ideal.
	  Example     
	       B = QQ[a..d];
	       J = ideal vars B;
	       C = complete res monomialCurveIdeal(B,{1,3,4});
	       K = filteredComplex(J,C,4);
	  Text
	       We compute the degree $0$ piece of the $E^3$ page below.
	  Example
	       E = prune spectralSequence K;
	       hilbertPolynomial(E^3)
///

doc ///
     Key
  	  (chainComplex, FilteredComplex)
     Headline
     	  the ambient chain complex of a filtered complex
     Usage
     	  C = chainComplex K
     Inputs
     	  K:FilteredComplex
     Outputs
     	  C:ChainComplex
     Description
     	  Text 
	       Returns the ambient chain complex of the filtered complex.
	  Example
	      A = QQ[x,y];
	      C = koszul vars A
	      K = filteredComplex C;
	      chainComplex K
	      K_infinity     
    SeeAlso
    	(symbol _, FilteredComplex, ZZ)
	(symbol _, FilteredComplex, InfiniteNumber)
	(symbol ^, FilteredComplex, ZZ)
	(symbol ^, FilteredComplex, InfiniteNumber)	       	       	       	       
///

doc ///
     Key
  	  (minimalPresentation, SpectralSequence)
	  (prune, SpectralSequence)
     Headline
     	  a minimal presentation of a spectral sequence
     Usage
     	  E = minimalPresentation e
     Inputs
     	  e:SpectralSequence
     Outputs
     	  E:SpectralSequence
     Description
     	  Text
	       Returns the minimal presentation of a spectral sequence.
	       
	       If we fail to prune a spectral sequence then the out-put can be highly
	       unintelligible.
	       
	       As a specific example consider the filtered complex $K$ below, obtained by multiplying the minimal free resolution of
	       the rational quartic space curve by successive powers of the irrelevant ideal.
	  Example     
	       B = QQ[a..d];
	       J = ideal vars B;
	       C = complete res monomialCurveIdeal(B,{1,3,4});
	       K = filteredComplex(J,C,4);
	  Text
	       Compare some pages of the non-pruned version of the spectral sequence
	       with that of the pruned version.
	  Example
	       E = prune spectralSequence K;
	       e = spectralSequence K;
	       e^3
	       E^3
     SeeAlso
     	 (minimalPresentation, SpectralSequencePage)
	 (prune, SpectralSequencePage)
     	 minimalPresentation
	 prune
	 pruningMaps	       
///

doc ///
     Key
  	  (minimalPresentation, SpectralSequencePage)
	  (prune, SpectralSequencePage)
     Headline
     	  a minimal presentation of a spectral sequence page
     Usage
     	  E = minimalPresentation e
     Inputs
     	  e:SpectralSequencePage
     Outputs
     	  E:SpectralSequencePage
     Description
     	  Text 
	       Returns a minimal presentation of the spectral sequence page.

	       If we fail to prune a spectral sequence then the out-put can be highly
	       unintelligible.
	       
	       As a specific example consider the filtered complex $K$ below, obtained by multiplying the minimal free resolution of
	       the rational quartic space curve by successive powers of the irrelevant ideal.
	  Example     
	       B = QQ[a..d];
	       J = ideal vars B;
	       C = complete res monomialCurveIdeal(B,{1,3,4});
	       K = filteredComplex(J,C,4);
	  Text
	       Compare some pruned and non-prunded pages the spectral sequence $E$ below.
	  Example
	       E = spectralSequence K;
	       E^3 
	       prune E^3
	       
     SeeAlso
     	 (minimalPresentation, SpectralSequence)
	 (prune, SpectralSequence)
     	 minimalPresentation
	 prune	       	       
///


doc ///
     Key
     	  (spectralSequence, FilteredComplex)
	 -- spectralSequence
     Headline
     	  construct a spectral sequence from a filtered complex
     Usage
     	  E = spectralSequence K
     Inputs
     	  K:FilteredComplex
	       A filtered complex
     Outputs
     	  E:SpectralSequence
     Description
     	  Text 
	       Returns the spectral sequence associated to the filtered complex.
	  Example
	      A = QQ[x,y];
	      C = koszul vars A
	      K = filteredComplex C;
	      E = spectralSequence K
///
    
doc ///
     Key
     	   (Hom, FilteredComplex, ChainComplex)
	   (Hom, ChainComplex, FilteredComplex)
     Headline
     	  the filtered Hom complex
     Usage
     	  f = Hom(K,C)
     Inputs
     	  K:FilteredComplex
	  C:ChainComplex
     Outputs
     	  f:FilteredComplex
     Description
     	  Text 
	      Returns the filtrations of the Hom complex determined by the double complex.  Here is 
	      an example which illustrates the syntax. 
	  Example
	     A = QQ[x,y,z,w];
	     B = res monomialCurveIdeal(A, {1,2,3});
	     C = res monomialCurveIdeal(A, {1,3,4});
	     F' = Hom(filteredComplex B, C)
	     F'' = Hom(B,filteredComplex C)   
     SeeAlso
     	 "Filtrations and tensor product complexes"	       
///
    
doc ///
     Key
     	   (chainComplex, SpectralSequence)
     Headline
     	  the underlying chain complex of a Spectral Sequence
     Usage
     	  K = chainComplex E
     Inputs
     	  E:SpectralSequence
     Outputs
     	  K:ChainComplex
     Description
     	  Text 
	       Returns the underlying chain complex of a spectral sequence.
	  Example
	      A = QQ[x,y];
	      C = koszul vars A
	      K = filteredComplex C;
	      E = spectralSequence K
	      chainComplex E
///

doc ///
     Key
     	  (spectralSequencePage, FilteredComplex, ZZ)
	 -- spectralSequencePage
     Headline
     	  construct a spectral sequence page from a filtered complex
     Usage
     	  E = spectralSequencePage(K,r)
     Inputs
     	  K:FilteredComplex
	       A filtered complex
	  r:ZZ
     Outputs
     	  E:SpectralSequencePage
     Description
     	  Text 
	       Returns the rth page of the spectral sequence determined by K.

               Consider the filtered complex $K$ below, obtained by multiplying the minimal free resolution of
	       the rational quartic space curve by successive powers of the irrelevant ideal.
	  Example     
	       B = QQ[a..d];
	       J = ideal vars B;
	       C = complete res monomialCurveIdeal(B,{1,3,4});
	       K = filteredComplex(J,C,4);
	       
	  Text
	       Let $E$ be the spectral sequence determined by $K$.
	  Example
	       E = spectralSequence K;
	  Text
	       We now compute some pages.
	  Example
	       E^0
	       E^1
	       E^infinity      
     SeeAlso
     	   "Examples of filtered complexes and spectral sequences"            
///
 


doc ///
     Key 
         netPage
	 (netPage,Page,List,List)
     Headline 
         display a small portion of a given Spectral Sequence page
     Usage 
         E' = netPage(E,L1,L2)
     Inputs 
	  E: Page
	  L1: List
	  -- A list {minP,minQ}, the bottom left corner coordinates to display
	  L2: List
	   -- A list {maxP,maxQ}, the top right corner coordinates to display
     Outputs
          E': Net
     Description	  
     	  Text
	     Produces the portion of a given spectral sequence page that lies in the square
	   with given bottom right and top left coordinates. 
	  Text
	  Example
	    R = QQ[x];
	    S = R/ideal"x2";
	    N = S^1/ideal"x";
	    M = R^1/R_0;
	    C = res M;
	    C' = C ** S;
	    D = res(N,LengthLimit => 10);
	    E0 = C' ** (filteredComplex D);
	    E = prune spectralSequence E0;
 	  Text
	     The E_2 page has nonzero E_2^{p,q} when 0 <= p <= 10 and 0 <= q <= 1,
	     so we may ask to restrict the display to 2 <= p <= 6 and 0 <= q <= 1.
	   
	    netPage(E_2,{2,0},{6,1})
	  Text
	       If we ask for a square that is too large, only the relevant portion of the page will be displayed.
	  Example
	    R = QQ[x];
	    S = R/ideal"x2";
	    N = S^1/ideal"x";
	    M = R^1/R_0;
	    C = res M;
	    C' = C ** S;
	    D = res(N,LengthLimit => 10);
	    E0 = C' ** (filteredComplex D);
	    E = prune spectralSequence E0;
	    netPage(E_2,{-5,0},{7,1})
/// 

    
doc ///
     Key
     	  (symbol _, SpectralSequence, ZZ)
     Headline
     	  the kth page of a spectral sequence
     Usage
     	  P = E_k
     Inputs
     	  E:SpectralSequence
	  k:ZZ
     Outputs
     	  P: SpectralSequencePage
     Description
     	  Text 
	       Returns the kth page of the spectral sequence determined by K.

               Consider the filtered complex $K$ below, obtained by multiplying the minimal free resolution of
	       the rational quartic space curve by successive powers of the irrelevant ideal.
	  Example     
	       B = QQ[a..d];
	       J = ideal vars B;
	       C = complete res monomialCurveIdeal(B,{1,3,4});
	       K = filteredComplex(J,C,4);
	       
	  Text
	       Let $E$ be the spectral sequence determined by $K$.
	  Example
	       E = spectralSequence K;
	  Text
	       We now compute some pages.
	  Example
	       E_0
	       E_1
	       E_infinity      
     SeeAlso
     	   (symbol ^,SpectralSequence,ZZ)
     	   "Examples of filtered complexes and spectral sequences"            	      
///

doc ///
     Key
     	  (symbol _, SpectralSequencePageMap, List)
     Headline
     	  The p,q th map on of a spectral sequence page 
     Usage
     	  d = D _L
     Inputs
     	  D:SpectralSequencePageMap
	  L:List
	      A list L = \{p,q\} \ of integers.
     Outputs
     	  d: Matrix
     Description
     	  Text 
	      Returns the p,q th map on a (lower index) spectral sequence page.  The relationship
	      $D_{p,q} = D^{-p,-q}$ holds.
	      
	      Consider the filtered complex $K$ below, obtained by multiplying the minimal free resolution of
	      the rational quartic space curve by successive powers of the irrelevant ideal.
	  Example 
	       B = QQ[a..d];
	       J = ideal vars B;
	       C = complete res monomialCurveIdeal(B,{1,3,4});
	       K = filteredComplex(J,C,4);
	  Text
	       We compute a map on the third page of the spectral sequence associated to $K$.
	  Example          
	       E = spectralSequence K
	       E^3 .dd_{-1,2}
     SeeAlso
     	  (symbol ^, SpectralSequencePageMap, List)
	  "Examples of filtered complexes and spectral sequences"            	      	      	  	      
///

doc ///
     Key
     	  (symbol ^, SpectralSequencePageMap, List)
     Headline
     	  the p,q th map on of a spectral sequence page 
     Usage
     	  d = D ^L
     Inputs
     	  D:SpectralSequencePageMap
	  L:List
	      A list L = \{p,q\} \ of integers.
     Outputs
     	  d: Matrix
     Description
     	  Text 
	      Returns the p,q th map on an (upper index) spectral sequence page.  The relationship $D^{p,q} = D_{-p,-q}$ holds.    
               
	       Consider the filtered complex $K$ below, obtained by multiplying the minimal free resolution of
	       the rational quartic space curve by successive powers of the irrelevant ideal.
	  Example 
	       B = QQ[a..d];
	       J = ideal vars B;
	       C = complete res monomialCurveIdeal(B,{1,3,4});
	       K = filteredComplex(J,C,4);
	  Text
	       We compute a map on the third page of the spectral sequence associated to $K$.
	  Example          
	       E = spectralSequence K
	       E_3 .dd^{1,-2}
     SeeAlso
      	  (symbol _, SpectralSequencePageMap, List)
	  "Examples of filtered complexes and spectral sequences"            	      	      
///
         
     
doc ///
     Key
     	  (symbol ^, SpectralSequence, ZZ)
     Headline
     	  the kth page of a spectral sequence
     Usage
     	  P = E^k
     Inputs
     	  E:SpectralSequence
	  k:ZZ
     Outputs
     	  P:SpectralSequencePage
     Description
     	  Text 
	      Returns the kth page of the spectral sequence.
              
	      Consider the filtered complex $K$ below, obtained by multiplying the minimal free resolution of
	      the rational quartic space curve by successive powers of the irrelevant ideal.
	  Example     
	       B = QQ[a..d];
	       J = ideal vars B;
	       C = complete res monomialCurveIdeal(B,{1,3,4});
	       K = filteredComplex(J,C,4);
	       
	  Text
	       Let $E$ be the spectral sequence determined by $K$.
	  Example
	       E = spectralSequence K;
	  Text
	       We now compute some pages.
	  Example
	       E^0
	       E^1
	       E^infinity      
     SeeAlso
     	   (symbol _, SpectralSequence, ZZ)
     	   "Examples of filtered complexes and spectral sequences"            	      	      
///

doc ///
     Key
	  (symbol ^, SpectralSequence, InfiniteNumber)
	  (symbol _, SpectralSequence, InfiniteNumber)
     Headline
     	  the infinity page of a spectral sequence
     Usage
     	  P = E^k
     Inputs
     	  E:SpectralSequence
	  k:InfiniteNumber
     Outputs
	  P:SpectralSequencePage
     Description
     	  Text 
	      Returns the infinity page a spectral sequence.
///

doc ///
     Key
     	  (symbol ^, SpectralSequencePage, List)
     Headline
     	  the module in the i,j position on the page
     Usage
     	  M = P^L
     Inputs
     	  P:SpectralSequencePage
	  L:List
	       A list L = \{i,j\} of integers
     Outputs
     	  M:Module
     Description
     	  Text 
	       Returns the module in the \{i,j\}  \ position in the spectral sequence page. 
	       (Using cohomological or upper indexing conventions.)  The relationship $E^{-i,-j} = E_{i,j}$ holds.
	  Example
	      A = QQ[x,y]
	      C = koszul vars A;
	      K = filteredComplex C;
	      E = spectralSequence K
	      E_0
	      E_0 ^{-1,0}
	      E^0 _{1,0}
     SeeAlso
     	  "Examples of filtered complexes and spectral sequences"            	      	      	      
///

doc ///
     Key
     	  (symbol _, SpectralSequencePage, List)
	  
     Headline
     	  the module in the i,j position on the page
     Usage
     	  M = P_L
     Inputs
     	  P:SpectralSequencePage
	  L:List
	       A list L = \{i,j\} \ of integers
     Outputs
     	  M:Module
     Description
     	  Text 
	       Returns the module in the \{i,j\} \ position in the spectral sequence page. 
	       (Using homological or lower indexing conventions.)  The relationship $E_{i,j} = E^{-i,-j}$ holds.
	  Example
	      A = QQ[x,y]
	      C = koszul vars A;
	      K = filteredComplex C;
	      E = spectralSequence K
	      E^0
	      E^0 _{1,0}
	      E_0 ^{-1,0}
     SeeAlso
     	  "Examples of filtered complexes and spectral sequences"            	      	      	      
///


doc ///
     Key
     	   (symbol **, ChainComplex, FilteredComplex)
	   (symbol **, FilteredComplex, ChainComplex)
     Headline
     	  filtered tensor product of complexes
     Usage
     	  KK = C ** K
	  KK = K ** C
     Inputs
     	  C:ChainComplex
	  K:FilteredComplex
     Outputs
     	  KK:FilteredComplex
     Description
     	  Text 
	       Returns the two filtrations of the tensor product complex determined by 
	       the double complex. 
	       The following example illustrates the syntax.
	  Example
	      A = QQ[x,y];
	      B = koszul vars A;
	      C = koszul vars A;
	      F' = (filteredComplex B) ** C
	      F'' = B ** (filteredComplex C)             
     SeeAlso
          "Filtrations and tensor product complexes"	       
///
    
doc ///
     Key
     	  (tensor, RingMap, ChainComplex)
     Headline
     	  tensor product of a chain complex by a ring map
     Usage
     	  D = tensor(f,C)
     Inputs
	  f:RingMap
	  C:ChainComplex
     Outputs
     	  D:ChainComplex
     Description
     	  Text 
	       Given a ring map R -> S and a chain complex over R, 
	       returns the tensor product of the given chain complex.
	  Example
	      R = QQ[x];
	      M = R^1/(x^2);
	      S = R/(x^4);
     	      C = res M
	      f = map(S,R,{1});
	      tensor(f,C)            
     SeeAlso
          "Filtrations and tensor product complexes"	       
///
        
    
    
doc ///
     Key
     	  (inducedMap, FilteredComplex, ZZ)
     Headline
     	  the i th inclusion map in a filtered complex
     Usage
     	  f = inducedMap(K,i)
     Inputs
     	  K:FilteredComplex
	  i:ZZ
     Outputs
     	  f:ChainComplexMap
     Description
     	  Text 
	       Returns the chain complex map specifying the inclusion of the i piece 
	       of the filtered
	       complex to the ambient chain complex.
	  Example
	      A = QQ[x,y];
	      C = koszul vars A;
	      K = filteredComplex C
	      inducedMap(K,1)     
///
    
doc ///
     Key
          (symbol _, FilteredComplex, ZZ)
	  (symbol _, FilteredComplex, InfiniteNumber)
     Headline
     	  the filtered pieces
     Usage
     	  C = K _ j
     Inputs
     	  K:FilteredComplex
	  j:ZZ 
	       an integer, infinity, or -infinity
     Outputs
     	  C:ChainComplex
     Description
     	  Text 
	       Returns the chain complex in (homological) filtration degree j.  
	       The relationship	$K _ j = K ^{(-j)}$ holds.     
    	   Example
	       A = QQ[x,y];
	       C = koszul vars A;
	       K = filteredComplex C
	       K_0
	       K_1
	       K_2
	       K^(-1)
	       K^(-2)
	       K_infinity
	       K_(-infinity)
	       K^(- infinity)
	       K^infinity       
     SeeAlso    
     	  (symbol ^, FilteredComplex, ZZ)  
	  (symbol ^, FilteredComplex, InfiniteNumber)         
///

doc ///
     Key
          (symbol ^, FilteredComplex, ZZ)
	  (symbol ^, FilteredComplex, InfiniteNumber)
     Headline
     	  the filtered pieces
     Usage
     	  C = K ^  j
     Inputs
     	  K:FilteredComplex
	  j:ZZ 
	       an integer, infinity, or -infinity
     Outputs
     	  C:ChainComplex
     Description
     	  Text 
	       Returns the chain complex in (cohomological) filtration degree j.
	       The relationship $K ^ j = K _{(-j)}$ holds.
	  Example
	       A = QQ[x,y];
	       C = koszul vars A;
	       K = filteredComplex C
	       K_0
	       K_1
	       K_2
	       K^(-1)
	       K^(-2)
	       K_infinity
	       K_(-infinity)
	       K^(-infinity)
	       K^infinity            
     SeeAlso
     	  (symbol _, FilteredComplex, ZZ)
	  (symbol _, FilteredComplex, InfiniteNumber)	       
///


doc ///
     Key
     	  connectingMorphism
     Headline
          use spectral sequences to compute connecting morphisms
     Usage 
         g = connectingMorphism(f, n)
     Inputs
         f:ChainComplexMap
	 n:ZZ	 
     Outputs
         g:Matrix 
     Description
          Text
	       Given a morphism $f: A \rightarrow B$ of chain complexes
	       returns the connecting map $H_{n+1}(coker f) \rightarrow H_n (im f)$.
///

doc ///
     Key
     	  (connectingMorphism, ChainComplexMap,ZZ)
     Headline
          use spectral sequences to compute connecting morphisms
     Usage 
         g = connectingMorphism(f, n)
     Inputs
         f:ChainComplexMap
	 n:ZZ	 
     Outputs
         g:Matrix 
     Description
          Text
	       Given a morphism $f: A \rightarrow B$ of chain complexes
	       returns the connecting map $H_{n+1}(coker f) \rightarrow H_n (im f)$.
///

doc ///
    	  Key
	    homologyIsomorphism
	  Headline 
	    compute the homology isomorphism 
	  Description
	       Text
	         Computes the isomorphism $ker d^r_{p,q} / image d^r_{p + r, q - r + 1} \rightarrow E^{r+1}_{p,q}$  
	  SeeAlso
	   (homologyIsomorphism, SpectralSequence, ZZ, ZZ, ZZ)      	
///	       


doc ///
     Key
     	  (homologyIsomorphism, SpectralSequence, ZZ, ZZ, ZZ)
     Headline
          the homology isomorphism
     Usage 
         g = homologyIsomorphism(SpectralSequence, ZZ, ZZ, ZZ)
     Inputs
         E:SpectralSequence
	 p:ZZ
	 q:ZZ
	 r:ZZ	 
     Outputs
         g:Matrix 
     Description
          Text
	       Computes the isomorphism $ker d^r_{p,q} / image d^r_{p + r, q - r + 1} \rightarrow E^{r+1}_{p,q}$
     	  Example
	       A = ZZ [s,t,u,v,w] ;
	       K = filteredComplex(reverse {simplicialComplex {s}, simplicialComplex {s,t}, simplicialComplex {s,t,u}, simplicialComplex {s*t, u}, simplicialComplex {s*t, u, v}, simplicialComplex {s*t, u, v, w}, simplicialComplex {s*t, s*w ,u, v}, simplicialComplex {s*t, s*w ,t * w, u, v}, simplicialComplex {s*t, s*w ,t * w, u * v}, simplicialComplex {s*t, s*w ,t * w, u * v, s * v}, simplicialComplex {s*t, s*w ,t * w, u * v, s * v, s*u}, simplicialComplex {s*t, s*w ,t * w, u * v, s * v, s*u, u * w}, simplicialComplex {s*t, s*w ,t * w, u * v, s * v, s*u, u * w, t* u}, simplicialComplex {s*t, s*w ,t * w, u * v, s * v, s*u, u * w, t* u, t*u*w}, simplicialComplex {s*t, s*w ,t * w, u * v, s * v, s*u, u * w, t* u, t*u*w, s*u*w}, simplicialComplex {s*t, s*w ,t * w, u * v, s * v, s*u, u * w, t* u, t*u*w, s*u*w,s*t*u}, simplicialComplex {s*t, s*w ,t * w, u * v, s * v, s*u, u * w, t* u, t*u*w, s*u*w,s*t*u, s*u*v}, simplicialComplex {s*t, s*w ,t * w, u * v, s * v, s*u, u * w, t* u, t*u*w, s*u*w,s*t*u, s*u*v, s*t*w}}, ReducedHomology => false);
	       E = prune spectralSequence K
	       e = spectralSequence K
	       apply(keys support E^11, i -> homologyIsomorphism(E, i#0, i#1, 11))
	       apply(keys support e^11, i -> homologyIsomorphism(e, i#0, i#1, 11))
     SeeAlso
     	 homologyIsomorphism	       
///

doc ///
     Key
          "filtered complexes and spectral sequences from simplicial complexes"
     Description
     	  Text	 	    
	    To make a filtered complex from a list of simplicial 
     	    complexes we first need to make some simplicial complexes.
     	  Example
	      R = QQ[x,y,z,w]; 	     
	      a = simplicialComplex {x*y*z, x*y, y*z, w*z}
	      b = simplicialComplex {x*y, w}
	      c = simplicialComplex {x,w}
	  Text 
	     Note that $b$ is a simplicial subcomplex of $a$ and that
	     $c$ is a simplicial subcomplex of $b$.
	     Let's now create a filtered complex.
	  Example
	      K = filteredComplex{a,b,c}	
	  Text
	      The associated spectral sequence takes the form:
     	  Example
	       E = spectralSequence K
     	  Text
	       Let's view some pages and maps of these pages. 
     	  Example
	       E^0
	       F0 = minimalPresentation(E^0) 
	       E^0 .dd
	       F0.dd 	      
	       E^1
	       F1 = minimalPresentation(E^1)
	       E^1 .dd
	       F1.dd
	       E^2
	       F2 = minimalPresentation(E^2) 
	       E^2 .dd
	       F2.dd
	       E^infinity
    	       (prune E) ^infinity
	  Text
     	     If we want the resulting complexes to correspond to the non-reduced homology
     	     of the simplicial complexes we set the ReducedHomology option
	     to false.
     	  Example 
	     J = filteredComplex({a,b,c}, ReducedHomology => false)           	     
	  Text
	       The resulting spectral sequence looks like
     	  Example
	       D = spectralSequence J
	       D^0
	       G0 = minimalPresentation(D^0)
	       G0.dd
	       D^1
	       G1 = minimalPresentation(D^1)
	       G1.dd
	       D^2
	       G2 = minimalPresentation(D^2)
	       G2.dd
	       D^infinity 	             	          
///	 
  
doc ///
     Key
          filteredHomologyObject
     Headline
     	  compute the filtered homology object
     SeeAlso
        (filteredHomologyObject, ZZ, ZZ, FilteredComplex)
	(associatedGradedHomologyObject, ZZ, ZZ, FilteredComplex)	  	  
///   

doc ///
     Key
	  (filteredHomologyObject, ZZ, ZZ, FilteredComplex)
     Headline 
	  compute the filtered homology object 
     Usage 
         M = filteredHomologyObject(ZZ, ZZ, FilteredComplex)
     Inputs
	 p:ZZ
	 n:ZZ
	 K:FilteredComplex	 
     Outputs
         M:Module 
     Description
	  Text
	       Computes the filtered homology object determined by the filtered chain complex
     SeeAlso
	(associatedGradedHomologyObject, ZZ, ZZ, FilteredComplex)	  	  
	       
///
   
--doc ///
--     Key
--          associatedGradedHomologyObject     	  
--///   
   	       
doc ///
     Key  
          associatedGradedHomologyObject
	  (associatedGradedHomologyObject, ZZ, ZZ, FilteredComplex)
     Headline 
	  compute the associated graded homology object 
     Usage 
         M = associatedGradedHomologyObject(ZZ, ZZ, FilteredComplex)
     Inputs
	 p:ZZ
	 n:ZZ
	 K:FilteredComplex	 
     Outputs
         M:Module 
     Description
	  Text
	       Computes the associated graded homology object determined by the filtered chain complex
///	       
  	  
doc ///
     Key
     	  "Edge homomorphisms"
     Description
     	  Text
	       Suppose that $E$ is a spectral sequence with the properties that:
	       
	       1. $E^2_{p,q} = 0$ for all $p < l$ and all $q \in \mathbb{Z}$;  
	       
	       2. $E^2_{p,q} = 0 $ for all $q < m$ and all $p \in \mathbb{Z}$;
	       
	       3.  $E$ converges to the graded module $\{H_n\}$ for $n \in \mathbb{Z}$.
	       
	       Then $E$ determines a $5$-term exact sequence
	       $H_{l+m+2} \rightarrow E^2_{l+2,m} \rightarrow E^2_{l,m+1} \rightarrow H_{l+m+1} \rightarrow E^2_{l+1,m} \rightarrow 0$ which we refer to as the 
	       {\it edge complex}. 
	       
	       Note that the above properties are satisfied if $E$ is the spectral sequence determined by a bounded filtration of a bounded chain complex.
	       
	       The following is an easy example, of a spectral sequence which arises from a nested chain of simplicial complexes, which illustrates this concept.
	       
	  Example
	       A = QQ[a,b,c,d];
       	       D = simplicialComplex {a*d*c, a*b, a*c, b*c};
	       F2D = D;
	       F1D = simplicialComplex {a*c, d};
	       F0D = simplicialComplex {a,d};
	       K = filteredComplex({F2D, F1D, F0D},ReducedHomology => false);
	       C = K_infinity;
	       prune HH C
    	  Text
	       The second page of the corresponding spectral sequences take the form:
	  Example     		   
	       E = spectralSequence(K);
	       e = prune E;
	       E^2
	       e^2
    	  Text
	       The acyclic edge complex for this example has the form
	       $H_1(C) \rightarrow E^2_{2,-1} \rightarrow E^2_{0,0} \rightarrow H_0(C)  \rightarrow E^2_{1, -1} \rightarrow 0$
    	       and is given by 
	  Example     
	       edgeComplex E
	       prune edgeComplex E
    	  Text
	       To see that it is acyclic we can compute
	  Example     
	       prune HH edgeComplex E
     Caveat
	  The method currently does not support pruned spectral sequences.
     SeeAlso
     	   "Examples of filtered complexes and spectral sequences"            	      
     	  	  
///
	  

doc ///
     Key 
     	  edgeComplex
	  (edgeComplex,SpectralSequence)
     Headline 
     	  the edge homomorphisms
     Usage 
         C = edgeComplex E 
     Inputs 
	  E: SpectralSequence
     Outputs
          C: ChainComplex
     Description
     	  Text
	       Suppose that $E$ is a spectral sequence with the properties that:
	       
	       1. $E^2_{p,q} = 0$ for all $p < l$ and all $q \in \mathbb{Z}$;  
	       
	       2. $E^2_{p,q} = 0 $ for all $q < m$ and all $p \in \mathbb{Z}$;
	       
	       3.  $E$ converges to the graded module $\{H_n\}$ for $n \in \mathbb{Z}$.
	       
	       Then $E$ determines a $5$-term exact sequence
	       $H_{l+m+2} \rightarrow E^2_{l+2,m} \rightarrow E^2_{l,m+1} \rightarrow H_{l+m+1} \rightarrow E^2_{l+1,m} \rightarrow 0$ which we refer to as the 
	       {\it edge complex}. 
	       
	       Note that the above properties are satisfied if $E$ is the spectral sequence determined by a bounded filtration of a bounded chain complex.
	       
	       The following is an easy example, of a spectral sequence which arises from a nested chain of simplicial complexes, which illustrates this concept.
	       
	  Example
	       A = QQ[a,b,c,d];
       	       D = simplicialComplex {a*d*c, a*b, a*c, b*c};
	       F2D = D;
	       F1D = simplicialComplex {a*c, d};
	       F0D = simplicialComplex {a,d};
	       K = filteredComplex({F2D, F1D, F0D},ReducedHomology => false);
	       C = K_infinity;
	       prune HH C
    	  Text
	       The second page of the corresponding spectral sequences take the form:
	  Example     		   
	       E = spectralSequence(K);
	       e = prune E;
	       E^2
	       e^2
    	  Text
	       The acyclic edge complex for this example has the form
	       $H_1(C) \rightarrow E^2_{2,-1} \rightarrow E^2_{0,0} \rightarrow H_0(C)  \rightarrow E^2_{1, -1} \rightarrow 0$
    	       and is given by 
	  Example     
	       edgeComplex E
	       prune edgeComplex E
    	  Text
	       To see that it is acyclic we can compute
	  Example     
	       prune HH edgeComplex E
     Caveat
	  The method currently does not support pruned spectral sequences.
///

doc ///
     Key
     	  (filteredComplex, Ideal, ChainComplex, ZZ)
     Headline
     	  I-adic filtrations of chain complexes
     Usage 
         K = filteredComplex(I,C,n)  
     Inputs 
	  I: Ideal
	  C: ChainComplex
	  n: ZZ
     Outputs
          K: FilteredComplex
     Description
     	 Text
	      By multiplying a chain complex by successive powers of an ideal we obtain a filtered complex.  
	 Example     
	      B = QQ[a..d]
	      J = ideal vars B
	      C = complete res monomialCurveIdeal(B,{1,3,4})
	      K = filteredComplex(J,C,4)
	 Text
	      Here are higher some pages of the associated spectral sequence:
	 Example
	       e = prune spectralSequence K
	       e^2
--	       e^3
--	       e^3 .dd
--	       e^4
--	       e^4 .dd
	       assert(all(keys support e^0, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,0)))
	       assert(all(keys support e^1, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,1)))
	       assert(all(keys support e^2, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,2)))
	       assert(all(keys support e^3, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,3)))
	       assert(all(keys support e^4, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,4)))
///


doc ///
     Key
     	  "Example 1"
     Headline
     	  Easy example of a filtered simplicial complex	  
     Description
     	  Text
	       Here we provide an easy example of a filtered simplicial complex and 
	       the resulting spectral sequence.  This example is small enough
	       that all aspects of it can be explicitly computed by hand.
	  Example
	       A = QQ[a,b,c,d];
	       D = simplicialComplex {a*d*c, a*b, a*c, b*c};
	       F2D = D
	       F1D = simplicialComplex {a*c, d}
	       F0D = simplicialComplex {a,d}
	       K= filteredComplex({F2D, F1D, F0D},ReducedHomology => false)
	       E = prune spectralSequence(K)
	       E^0
	       E^1
	       E^2
	       E^3
	       E^infinity
	       C = K_infinity
	       prune HH C
	       E^2 .dd
	  Text
	       Considering the $E^2$ and $E^3$ pages of the spectral sequence 
	       we conclude that the map $d^2_{2,-1}$ must have a $1$-dimensional
	       image and a $1$-dimensional kernel.  This can be verified easily:
	  Example
	      rank ker E^2 .dd_{2,-1}
	      rank image E^2 .dd_{2,-1}     
///     

-- We might want to not include this next example
doc ///
     Key
     	  "Example 2"
     Headline
     	  Easy example of a filtered simplicial complex
     Description
     	  Text
	       We provide an easy example of a filtered simplicial complex and
	       the resulting spectral sequence.  This example is small enough that
	       all aspects of it can be explicitly computed by hand.
	  Example
	       A = QQ[a,b,c];
	       D = simplicialComplex({a*b*c})
	       F3D = D;
	       F2D = simplicialComplex({a*b,a*c,b*c})
	       F1D = simplicialComplex({a*b,c})
	       F0D = simplicialComplex({a,b})
	       K = filteredComplex({F3D,F2D,F1D,F0D}, ReducedHomology => false)
	       E = prune spectralSequence K
	       E^0
	       E^0 .dd
	       E^0
	       E^1
	       E^0 .dd_{1,0}
	       E^1 .dd
	       E^1
	       E^0
	       E^2
	       prune HH K_infinity
    	       E^infinity
///

-- We might want to not include this next example
doc ///
     Key
     	  "Example 3"
     Headline
     	  Easy example of a filtered simplicial complex
     Description
     	  Text
	       We provide an easy example of a filtered simplicial complex
	       and the resulting spectral sequence.  This example is small enough that
	       all aspects of it can be explicitly computed by hand.	       
     	  Example
	       A = QQ[a,b,c]
	       D = simplicialComplex {a*b*c}
	       F2D = D
	       F1D = simplicialComplex {a*b,a*c,b*c}
	       F0D = simplicialComplex {a,b,c}
	       K = filteredComplex({F2D,F1D,F0D}, ReducedHomology => false)
	       C = K_infinity    	     
	       E = prune spectralSequence K
	       E^0
	       E^0 .dd	   
	       E^1
	       E^1 .dd
	       E^2
	       E^2 .dd
	       E^infinity
	       prune HH K_infinity
///	       



TEST ///
restart;
needsPackage "SpectralSequences";
A = QQ[a,b,c];
C = new ChainComplex;
C.ring = A;
K = filteredComplex C;
assert(K_0 == C);
assert(K_1 == C);
///    

TEST ///
restart;
needsPackage "SpectralSequences";
A = QQ[a,b,c];
D = simplicialComplex {a*b*c};
F2D = D;
F1D = simplicialComplex {a*b,a*c,b*c};
F0D = simplicialComplex {a,b,c};
K = filteredComplex({F2D,F1D,F0D}, ReducedHomology => false);
E = prune spectralSequence K;
e = spectralSequence K;
assert(all(keys support E^0, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,0)))
assert(all(keys support E^1, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,1)))
assert(all(keys support E^2, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,2)))
assert(all(keys support E^3, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,3)))
assert(all(keys support E^4, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,4)))
assert(all(keys support E^5, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,5)))
assert(all(keys support e^0, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,0)))
assert(all(keys support e^1, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,1)))
assert(all(keys support e^2, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,2)))
assert(all(keys support e^3, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,3)))
assert(all(keys support e^4, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,4)))
assert(all(keys support e^5, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,5)))
///

TEST ///
restart
needsPackage "SpectralSequences";
-- The following example is taken from p. 127, Fig 7.2 of 
-- Zomorodian's "Topology for computing"
A = ZZ [s,t,u,v,w] ;
d0 = simplicialComplex {s};
d1 = simplicialComplex {s,t} ;
d2 = simplicialComplex {s,t,u} ;
d3 = simplicialComplex {s*t, u} ;
d4 = simplicialComplex {s*t, u, v} ;
d5 = simplicialComplex {s*t, u, v, w} ;
d6 = simplicialComplex {s*t, s*w ,u, v} ;
d7 = simplicialComplex {s*t, s*w ,t * w, u, v} ;
d8 = simplicialComplex {s*t, s*w ,t * w, u * v};
d9 = simplicialComplex {s*t, s*w ,t * w, u * v, s * v};
d10 = simplicialComplex {s*t, s*w ,t * w, u * v, s * v, s*u};
d11 = simplicialComplex {s*t, s*w ,t * w, u * v, s * v, s*u, u * w};
d12 = simplicialComplex {s*t, s*w ,t * w, u * v, s * v, s*u, u * w, t* u};
d13 = simplicialComplex {s*t, s*w ,t * w, u * v, s * v, s*u, u * w, t* u, t*u*w};
d14 = simplicialComplex {s*t, s*w ,t * w, u * v, s * v, s*u, u * w, t* u, t*u*w, s*u*w};
d15 = simplicialComplex {s*t, s*w ,t * w, u * v, s * v, s*u, u * w, t* u, t*u*w, s*u*w,s*t*u};
d16 = simplicialComplex {s*t, s*w ,t * w, u * v, s * v, s*u, u * w, t* u, t*u*w, s*u*w,s*t*u, s*u*v};
d17 = simplicialComplex {s*t, s*w ,t * w, u * v, s * v, s*u, u * w, t* u, t*u*w, s*u*w,s*t*u, s*u*v, s*t*w};
L = reverse {d0, d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13, d14, d15, d16, d17};
K = filteredComplex (L, ReducedHomology => false);
E = prune spectralSequence K
e = spectralSequence K
assert(all(keys support E^0, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,0)))
assert(all(keys support E^1, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,1)))
assert(all(keys support E^2, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,2)))
assert(all(keys support E^3, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,3)))
assert(all(keys support E^4, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,4)))
assert(all(keys support E^5, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,5)))
assert(all(keys support E^6, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,6)))
assert(all(keys support E^7, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,7)))
assert(all(keys support E^8, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,8)))
assert(all(keys support E^9, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,9)))
assert(all(keys support E^10, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,10)))
assert(all(keys support E^11, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,11)))
assert(all(keys support E^12, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,12)))
assert(all(keys support e^0, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,0)))
assert(all(keys support e^1, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,1)))
assert(all(keys support e^2, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,2)))
assert(all(keys support e^3, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,3)))
assert(all(keys support e^4, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,4)))
assert(all(keys support e^5, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,5)))
assert(all(keys support e^6, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,6)))
assert(all(keys support e^7, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,7)))
assert(all(keys support e^8, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,8)))
assert(all(keys support e^9, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,9)))
assert(all(keys support e^10, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,10)))
assert(all(keys support e^11, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,11)))
assert(all(keys support e^12, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,12)))
///

TEST ///
restart
needsPackage "SpectralSequences";
A = QQ[a,b,c,d];
D = simplicialComplex {a*d*c, a*b, a*c, b*c};
F2D = D;
F1D = simplicialComplex {a*c, d};
F0D = simplicialComplex {a,d};
K = filteredComplex({F2D, F1D, F0D},ReducedHomology => false);
E = spectralSequence(K);
e = prune E;
assert(all(keys support E^0, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,0)))
assert(all(keys support E^1, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,1)))
assert(all(keys support E^2, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,2)))
assert(all(keys support E^3, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,3)))
assert(all(keys support E^4, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,4)))
assert(all(keys support E^5, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,5)))
assert(all(keys support E^6, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,6)))
assert(all(keys support E^7, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,7)))
assert(all(keys support E^8, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,8)))
assert(all(keys support E^9, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,9)))
assert(all(keys support E^10, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,10)))
assert(all(keys support E^11, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,11)))
assert(all(keys support E^12, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,12)))
assert(all(keys support e^0, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,0)))
assert(all(keys support e^1, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,1)))
assert(all(keys support e^2, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,2)))
assert(all(keys support e^3, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,3)))
assert(all(keys support e^4, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,4)))
assert(all(keys support e^5, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,5)))
assert(all(keys support e^6, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,6)))
assert(all(keys support e^7, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,7)))
assert(all(keys support e^8, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,8)))
assert(all(keys support e^9, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,9)))
assert(all(keys support e^10, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,10)))
assert(all(keys support e^11, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,11)))
assert(all(keys support e^12, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,12)))
///

TEST ///
restart
needsPackage "SpectralSequences";
B = QQ[a..d];
J = ideal vars B;
C = complete res monomialCurveIdeal(B,{1,3,4});
K = filteredComplex(J,C,4);
e = prune spectralSequence K;
assert(all(keys support e^0, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,0)))
assert(all(keys support e^1, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,1)))
assert(all(keys support e^2, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,2)))
assert(all(keys support e^3, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,3)))
assert(all(keys support e^4, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,4)))
///


TEST ///
restart
needsPackage "SpectralSequences";
S = ZZ/101[x,y];
I = ideal(x^2,x*y,y^2);
R = S/I;
kR = coker vars R;
kS = coker vars S;
CS = res kS;
CR = res(kR,LengthLimit=>6);
CS' = CS**R;
E = prune spectralSequence (CS' ** filteredComplex CR);
assert(all(keys support E^0, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,0)))
assert(all(keys support E^1, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,1)))
assert(all(keys support E^2, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,2)))
assert(all(keys support E^3, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,3)))
assert(all(keys support E^4, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,4)))
///
end

---
-- scratch code --
---

--------------------------------------------------------------------------------
restart
uninstallPackage"SpectralSequences"
installPackage"SpectralSequences"
installPackage("SpectralSequences", RemakeAllDocumentation => true)
check "SpectralSequences";
viewHelp SpectralSequences
------------------------------------------

Status API Training Shop Blog About
© 2016 GitHub, Inc. Terms Privacy Security Contact Help

