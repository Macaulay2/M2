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
    Headline => "spectral sequences and filtered complexes",
    Keywords => { "Homological Algebra" },
    Version  => "1.2",
    Date     => "9 July 2025",
    Authors  => {
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
    PackageExports => {"SimplicialComplexes", "Complexes", "PushForward"},
    AuxiliaryFiles => true,
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
    "page",
    "pruningMaps",
    "edgeComplex",
    "filteredHomologyObject",
    "associatedGradedHomologyObject",
    "netPage",
    }

importFrom_Core {
    "getAttribute", "hasAttribute", "ReverseDictionary",
    }

--------------------------------------------------------------------------------
-- CODE
--------------------------------------------------------------------------------
-- TODO: move these to Complexes

spots = method()
spots Complex := List => C -> sort keys C.module

support Complex := List => C -> select(spots C, i -> C_i != 0)

-- the following relies on the pushFwd method from the package "PushForward.m2"
pushFwd(RingMap, Complex) := o -> (f, C) -> (
    (lo, hi) := concentration C;
    if lo == hi
    then complex(pushFwd(f, C_lo, o), Base => lo)
    else complex applyValues(C.dd.map,
	m -> pushFwd(f, m, o)))

naiveTruncation(Complex, ZZ) := Complex => (C, n) -> (
    (lo, hi) := concentration C;
    if n > 0 then naiveTruncation(C, (lo + n,  infinity)) else
    if n < 0 then naiveTruncation(C, (-infinity, hi + n)) else C)

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
FilteredComplex _ ZZ := Complex => (K,p) -> (
    if K#?p then K#p
    else if p < min K then K#(min K)
    else if p > max K then K#(max K)
    )

FilteredComplex ^ InfiniteNumber :=
FilteredComplex ^ ZZ := Complex => (K,p) -> K_(-p)

complex FilteredComplex := Complex => {} >> o -> K -> K_infinity

-- Returns the inclusion map from the pth subcomplex to the top
protect inducedMaps
inducedMap (FilteredComplex, ZZ) := ComplexMap => opts -> (K,p) -> (
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
    if #L == 0 then error "expected at least one complex map or simplicial complex";
    if not uniform L then error "expected a list of complex maps or simplicial complexes";
    --
    maps := if instance(L#0, SimplicialComplex) then (
	kk := coefficientRing L#0;
	if opts.ReducedHomology then (
	    -- By default the ambient simplicial complex is the first element of the list
	    C := complex L#0;
	    apply(#L-1, p -> map(C, complex L#(p+1),
		    i -> sub(contract(transpose matrix{faces(i,L#0)}, matrix{faces(i,L#(p+1))}), kk))))
	else (
	    -- By default the ambient simplicial complex is the first element of the list
	    C = complex L#0;
	    C = naiveTruncation(C, 1);
	    apply(#L-1, p -> map(C, naiveTruncation(complex L#(p+1), 1),
		    i -> sub(contract(transpose matrix{faces(i,L#0)}, matrix{faces(i,L#(p+1))}), kk))))
	)
    else if instance(L#0, ComplexMap) then (
	-- By default the ambient chain complex is target of first map.
	C = target L#0;
	if same apply(L, target) then L
	else error "expected all maps to have the same target")
    else error "expected a list of complex maps or simplicial complexes";
    --
    Z := image map(C, C, i -> 0*id_(C_i)); -- make zero subcomplex as a subcomplex of ambient complex
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
filteredComplex Complex := FilteredComplex => opts -> C -> (
    (lo, hi) := concentration C;
    if lo == hi
    then filteredComplex{ map(C, image(0 * id_C), id_C) }
    else filteredComplex(Shift => -lo,
	apply(hi-lo, i -> inducedMap(C, naiveTruncation(C, lo, hi-i-1)))))

--produce the "x-filtration" of the tensor product complex.
xTensormodules := (p,q,T) -> (
    apply(indices T_q, components T_q,
	(ind, M) -> if ind#0 <= p
	then image id_M
	else image(0 * id_M)))

xTensorComplex := (T,p) ->(
    (lo, hi) := concentration T;
    if lo == hi
    then complex(directSum xTensormodules(p, lo, T), Base => lo)
    else complex applyPairs(T.dd.map,
	(i,f) -> i => inducedMap(directSum(xTensormodules(p, i-1, T)), directSum(xTensormodules(p, i, T)), f)))

FilteredComplex ** Complex := FilteredComplex => (K,C) -> (
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
yTensorModules := (p,q,T)->(
    apply(indices T_q, components T_q,
	(ind, M) -> if ind#1 <= p
	then image id_M
	else image(0 * id_M)))

yTensorComplex := (T,p) -> (
    (lo, hi) := concentration T;
    if lo == hi
    then complex(directSum(yTensorModules(p, lo, T), Base => lo))
    else complex applyPairs(T.dd.map,
	(i,f) -> i => inducedMap(directSum(yTensorModules(p, i-1, T)), directSum(yTensorModules(p, i, T)), f)))

Complex ** FilteredComplex := FilteredComplex => (C,K) -> (
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
xHomModules := (n, d, H)->(
    -- want components {p,q} = Hom(-p, q) with p + q = d and p <= n
    apply(indices H_d, components H_d,
	(ind, M) -> if -ind#0 <= n
	then image id_M
	else image(0 * id_M)))

xHomComplex := (T,n) -> (
    (lo, hi) := concentration T;
    if lo == hi
    then complex(directSum(xHomModules(n, lo, T), Base => lo))
    else complex applyPairs(T.dd.map,
	(i,f) -> i => inducedMap(directSum(xHomModules(n, i-1, T)), directSum(xHomModules(n, i, T)), f)))

-- produce the "x-filtration" of the Hom complex.
Hom (FilteredComplex, Complex):= FilteredComplex => opts -> (K, C) -> (
    supp := support K_infinity;
    -- try to handle the boundary cases --
    if supp != {} and #supp > 1 then (
	N := - max support K_infinity;
	P := - min support K_infinity;
	H := Hom(K_infinity, C, opts);
	filteredComplex(reverse for i from N to P - 1 list inducedMap(H, xHomComplex(H,i)),
	    Shift => - N)
	)
    else ( if #supp == 1 then
	(
	    p := min supp;
	    h := Hom(K_infinity, C, opts);
	    filteredComplex( {inducedMap(h, xHomComplex(h, p))}, Shift =>  p + 1 )
	    )
	else(
	    hhh := Hom(K_infinity, C, opts);
	    filteredComplex({id_hhh})
	    )
	)
    )

-- next are some functions used in the "y-filtration" of the Hom complex.

yHomModules := (n, d, H) -> (
    -- want components {p,q} = Hom(-p, q) with p + q = d and q <= n
    apply(indices H_d, components H_d,
	(ind, M) -> if ind#1 <= n
	then image id_M
	else image(0 * id_M)))

yHomComplex := (T,n) -> (
    (lo, hi) := concentration T;
    if lo == hi
    then complex(directSum(yHomModules(n, lo, T), Base => lo))
    else complex applyPairs(T.dd.map,
	(i,f) -> i => inducedMap(directSum(yHomModules(n, i-1, T)), directSum(yHomModules(n, i, T)), f)))

Hom (Complex, FilteredComplex) := FilteredComplex => opts -> (C, K) -> (
    supp := support K_infinity;
    -- try to handle the boundary cases --
    if supp != {} and #supp > 1 then (
	N :=  max support K_infinity;
	P :=  min support K_infinity;
	H := Hom(C, K_infinity, opts);
	filteredComplex(reverse for i from P to N - 1 list inducedMap(H, yHomComplex(H,i)),
	    Shift => - P)
	)
    else ( if #supp == 1 then
	(
	    p := min supp;
	    h := Hom(C, K_infinity, opts);
	    filteredComplex( {inducedMap(h, yHomComplex(h, p))}, Shift =>  - p  + 1 )
	    )
	else(
	    hhh := Hom(C, K_infinity, opts);
	    filteredComplex({id_hhh})
	    )
	)
    )


-- I-adic filtration code --
-- the following script allows us to multiply a chain complex by an ideal
Ideal * Complex := Complex => (I,C) -> (
    (lo, hi) := concentration C;
    if lo == hi
    then complex(I * C_lo, Base => lo)
    else complex applyValues(C.dd.map,
	f -> inducedMap(I * target f, I * source f, f)))

filteredComplex(Ideal,Complex,ZZ) := FilteredComplex => opts -> (I,C,n) ->(
    if n < 0 then error "expected a non-negative integer"
    else
    filteredComplex(apply(n, i -> inducedMap(C, I^(i+1) * C)), Shift => n)
    )

--------------------------------------------------------------------------------
-- Pages and Sequences
--------------------------------------------------------------------------------

Page = new Type of MutableHashTable
Page.synonym = "page"

Page.GlobalAssignHook = globalAssignFunction
Page.GlobalReleaseHook = globalReleaseFunction

new Page := Page => (cl) -> (
    C := newClass(Page, new MutableHashTable); -- sigh
    C.cache = new CacheTable;
    b := C.dd = new PageMap;
    b.degree = {};
    b.source = b.target = C;
    C)

ring   Page := C -> C.ring
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

-- printing
describe Page := E -> net expression E
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
    R := if E.?ring then E.ring else error "page does not have a ring";
    minP := L#0;
    maxP := L#1;
    minQ := M#0;
    maxQ := M#1;
    --  E := new Page;
    --  E.ring = A;
    for i from minP to maxP do
    for j from minQ to maxQ do E#{i,j} = R^0;
    E)

--------------------------------------------------------------------------------
-- PageMap
--------------------------------------------------------------------------------

PageMap = new Type of MutableHashTable
PageMap.synonym = "page map"

PageMap.GlobalAssignHook = globalAssignFunction
PageMap.GlobalReleaseHook = globalReleaseFunction

spots PageMap := List => d -> select(keys d,
    i -> class i === List and all(i, j -> class j === ZZ))

support PageMap := List => d -> sort select(spots d, i -> d#i != 0)

PageMap _ List := Matrix => (f,i) ->  if f#?i then f#i else (
    de := f.degree;
    so := (f.source)_i;
    ta := (f.target)_(i + de);
    map(ta,so,0))

-- printing
lineOnTop := (s) -> concatenate(width s : "-") || s

describe PageMap := d -> net expression d
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

--------------------------------------------------------------------------------
-- spectral sequences
--------------------------------------------------------------------------------

SpectralSequence = new Type of MutableHashTable
SpectralSequence.synonym = "spectral sequence"
SpectralSequence.GlobalAssignHook = globalAssignFunction
SpectralSequence.GlobalReleaseHook = globalReleaseFunction

-- printing
describe SpectralSequence := E -> net expression E
net SpectralSequence := E -> (
    if hasAttribute(E, ReverseDictionary)
    then toString getAttribute(E, ReverseDictionary)
    else net expression E)
expression SpectralSequence := E -> stack(
    "  .-.  ",
    " (o o) ",
    " | O \\   Unnamed spectral sequence! ..ooOOOooooOO",
    "  \\   \\  ",
    "   `~~~` ")

spectralSequence = method (Options => { Prune => false })
spectralSequence FilteredComplex := SpectralSequence => opts -> K -> (
    new SpectralSequence from {
	symbol filteredComplex => K,
	symbol cache => CacheTable,
	symbol Prune => opts.Prune}
    )

-- TODO: also cache E^infinity
SpectralSequence ^ InfiniteNumber :=
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
		    for q from -p + min K_(infinity) to max K_(infinity) + 1 do (
			H#{p,q} = if E.Prune then prune epq(K,p,q,s) else epq(K,p,q,s)
			);
		    );
		);
	    );
	H)
    else E#r ??= spectralSequencePage(E.filteredComplex,r, Prune => E.Prune)
    )

SpectralSequence _ InfiniteNumber :=
SpectralSequence _ ZZ := SpectralSequencePage => (E,r) -> ( E^r )

minimalPresentation SpectralSequence := prune SpectralSequence := SpectralSequence => opts -> E -> (
    spectralSequence(E.filteredComplex, Prune => true))

filteredComplex SpectralSequence := FilteredComplex => opts -> E -> E.filteredComplex
complex SpectralSequence := Complex => {} >> opts -> E -> complex E.filteredComplex

--------------------------------------------------------------------------------
-- spectral sequence pages
--------------------------------------------------------------------------------

SpectralSequencePage = new Type of Page
SpectralSequencePage.synonym = "spectral sequence page"
SpectralSequencePage.GlobalAssignHook = globalAssignFunction
SpectralSequencePage.GlobalReleaseHook = globalReleaseFunction

spectralSequencePage = method(Options => { Prune => false })
spectralSequencePage(FilteredComplex, ZZ) := SpectralSequencePage => opts ->  (K,r) -> (
    new SpectralSequencePage from {
	symbol filteredComplex => K,
	symbol number          => r,
	symbol dd              => spectralSequencePageMap(K, r, opts),
	symbol Prune           => opts.Prune,
	symbol cache           => CacheTable}
    )

minimalPresentation SpectralSequencePage := prune SpectralSequencePage := SpectralSequencePage  => opts -> E -> (
    spectralSequencePage(E.filteredComplex, E.number, Prune => true))

SpectralSequencePage _ List := Module => (E, i) -> source(E.dd_i)
SpectralSequencePage ^ List := Module => (E, i) -> E_(-i)

-- view the modules on a Spectral Sequence Page.  We are referring to these
-- as the support of the page.

page SpectralSequencePage := Page => opts -> E -> (
    K := E.filteredComplex;
    s := E.number;
    H := new Page;
    -- again trying to handle the case of the zero complex --
    if min K_(infinity) < infinity and max K_infinity > - infinity then (
	for p from min K to max K do (
	    for q from -p + min K_(infinity) to max K_(infinity) + 1 do (
		-- H#{p,q} = E^s_{p,q}
		H#{p,q} = if E.Prune then prune epq(K,p,q,s) else epq(K,p,q,s))
	    );
	);
    H)
page Page := Page => opts -> identity

-- the following two methods are used to view the modules
-- on the r th page in grid form.
-- this method is called in net of spectral sequence page.
-- it would be good to delete the zero rows.

net SpectralSequencePage := E -> (page E)

support SpectralSequencePage := E -> new Page from apply(spots E.dd, i-> i=> source E.dd #i)

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
    image K_(p+r-1).dd_(p+q+1) + K_(p-1) _ (p+q))

-- compute the pq modules on the rth page
epq = method()
epq(FilteredComplex, ZZ, ZZ, ZZ) := (K,p,q,r) -> (
    (cycles(K,p,q,r) + boundaries(K,p,q,r)) / boundaries(K,p,q,r))

-- the pq maps on the rth page.
epqrMaps = method()
epqrMaps(FilteredComplex, ZZ, ZZ, ZZ) := (K,p,q,r) -> (
    inducedMap(epq(K, p-r, q+r-1, r), epq(K,p,q,r), (K_infinity).dd_(p+q), Verify => false))

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
    f)

ErMaps = method(Options => {Prune => false})
ErMaps(FilteredComplex,ZZ,ZZ,ZZ) := Matrix => opts -> (K,p,q,r) -> (
    if opts.Prune then pruneEpqrMaps(K,p,q,r) else epqrMaps(K,p,q,r))

-- the homology at the pq spot on the rth page.
rpqHomology = method()
rpqHomology(SpectralSequence,ZZ,ZZ,ZZ) := (E,p,q,r) -> (
    ker E^r.dd_{p,q} / image E^r.dd_{p+r,q-r+1})

-- the isomorphism of the homology at the pq spot
-- on the r-th page and the module on at the pq spot on the r+1-th page.
homologyIsomorphism = method()
homologyIsomorphism(SpectralSequence,ZZ,ZZ,ZZ) := (E,p,q,r) -> (
    if E.Prune then rpqPruneIsomorphism(E,p,q,r)
    else inducedMap(
	source (E^(r+1) .dd_{p,q}), rpqHomology(E,p,q,r),
	-- FIXME: if Verify not set to false can get error when running on M2 1.9
	id_(E^(r+1) .filteredComplex _infinity _(p+q)), Verify => false))

rpqPruneIsomorphism = method()
rpqPruneIsomorphism(SpectralSequence,ZZ,ZZ,ZZ) := (E,p,q,r) -> (
    M := rpqHomology(E,p,q,r);
    f := inducedMap(
	target (E^(r + 1) .dd_{p,q}) .cache.sourcePruningMap, M,
	-- FIXME: if Verify not set to false can get error when running on M2 1.9
	(E^r .dd_{p,q}).cache.sourcePruningMap, Verify => false);
    inverse((E^(r + 1) .dd_{p,q}) .cache.sourcePruningMap) * f)

---
-- Spectral Sequence Page Maps
---

SpectralSequencePageMap = new Type of PageMap
SpectralSequencePageMap.synonym = "spectral sequence page map"
SpectralSequencePageMap.GlobalAssignHook = globalAssignFunction
SpectralSequencePageMap.GlobalReleaseHook = globalReleaseFunction

spectralSequencePageMap = method(Options =>{Prune => false})

-- FIXME
spectralSequencePageMap(FilteredComplex,ZZ) := SpectralSequencePageMap => opts -> (K,r) -> (
    myList := {};
    -- try to handle case coming from the zero complex --
    Kmin := min K_infinity; Kmax := max K_(infinity);
    if class Kmin < infinity  and Kmax > - infinity then (
	for p from min K to max K do (
	    for q from -p + min K_(infinity) to max K_(infinity) -p do (
		myList =
		append(myList, {p,q} => ErMaps(K,p,q,r, Prune => opts.Prune)) )); );
    new SpectralSequencePageMap from join(myList, {
	symbol degree          => {-r, r-1},
	symbol filteredComplex => K,
	symbol Prune           => opts.Prune,
	symbol cache           => new CacheTable,
	}
    )
)

SpectralSequencePageMap _ List := Matrix => (d,i)-> (
    if d#?i then d#i else
    if d.Prune
    then pruneEpqrMaps(d.filteredComplex,i#0,i#1,- d.degree #0)
    else epqrMaps(d.filteredComplex,i#0,i#1,- d.degree #0)
    )

SpectralSequencePageMap ^ List := Matrix => (d,i)-> (d_(-i))


-- auxiliary spectral sequence stuff.

-- given a morphism f: A --> B
-- compute the connecting map
-- HH_{n+1}( coker f) --> HH_n (im f)

connectingMorphism = method()
connectingMorphism(ComplexMap, ZZ) := (a, n) -> (
    K := filteredComplex ({a});
    e := spectralSequence K;
    e^1 .dd_{1, n})

hilbertPolynomial Page := Page => o -> E -> (
    P := new Page;
    apply(spots page E, i -> P#i = hilbertPolynomial(E_i));
    P)

pruningMaps = method()
pruningMaps SpectralSequencePage := E -> (
    if not E.Prune then error "page is not pruned";
    P := new PageMap;
    P.degree = E.dd.degree;
    apply(spots E.dd, i -> P#i = E.dd_i .cache.sourcePruningMap);
    P)

basis(ZZ,   SpectralSequencePage) :=
basis(List, SpectralSequencePage) := Page => opts -> (deg, E) -> (
    P := new Page;
    apply(spots E.dd, i -> P#i = basis(deg,E_i));
    P)

edgeComplex = method()
edgeComplex SpectralSequence := E -> (
    if E.Prune then error "not currently implemented for pruned spectral sequences";
    M := select(spots E^2 .dd, i -> E^2_i != 0);
    l := min apply(M, i -> i#0);
    m := min apply(M, i -> i#1);
    C := complex E;
    if M != {} then (
	complex {inducedMap(E^2_{l + 1, m}, HH_(l + m + 1) C, id_(C_(l + m + 1))),
	    inducedMap(HH_(l + m + 1) C, E^2_{l,m + 1}, id_(C_(l + m + 1))),
	    E^2 .dd_{l + 2,m}, inducedMap(E^2_{l + 2, m}, HH_(l + m + 2) C, id_(C_(l + m + 2)))})
    else complex C.ring)

filteredHomologyObject = method()
filteredHomologyObject(ZZ, ZZ, FilteredComplex) := (p,n,K) -> (
    image inducedMap(HH_n K_infinity, HH_n K_p, id_(K_infinity _n)))

associatedGradedHomologyObject = method()
associatedGradedHomologyObject(ZZ, ZZ, FilteredComplex) := (p,n,K) -> (
    filteredHomologyObject(p,n,K) / filteredHomologyObject(p-1,n,K))

---
-- Documentation and tests
---

beginDocumentation()

load "./SpectralSequences/docs.m2"
load "./SpectralSequences/examples.m2"
load "./SpectralSequences/tests.m2"

---
-- scratch code --
---

end--

--------------------------------------------------------------------------------
restart
uninstallPackage"SpectralSequences"
installPackage"SpectralSequences"
installPackage("SpectralSequences", RemakeAllDocumentation => true)
check "SpectralSequences";
viewHelp SpectralSequences
------------------------------------------
