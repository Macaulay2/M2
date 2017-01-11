-- -*- coding: utf-8 -*-
------------------------------------------------------------------------------
-- Copyright 2009, 2010, 2011, 2012, 2013, 2014  Gregory G. Smith
--
-- This program is free software: you can redistribute it and/or modify it
-- under the terms of the GNU General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option)
-- any later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
-- more details.
--
-- You should have received a copy of the GNU General Public License along
-- with this program.  If not, see <http://www.gnu.org/licenses/>.
------------------------------------------------------------------------------
newPackage(
  "NormalToricVarieties",
  AuxiliaryFiles => true,
  Version => "1.3",
  Date => "3 July 2014",
  Authors => {{
      Name => "Gregory G. Smith", 
      Email => "ggsmith@mast.queensu.ca", 
      HomePage => "http://www.mast.queensu.ca/~ggsmith"}},
  Headline => "normal toric varieties",
  PackageExports => {"Polyhedra"},
  PackageImports => {"FourierMotzkin","Normaliz"},
  DebuggingMode => false
  )

export { 
  "normalToricVariety", 
  "NormalToricVariety", 
  "WeilToClass",   
  "affineSpace",
  "projectiveSpace", 
  "weightedProjectiveSpace",   
  "hirzebruchSurface", 
  "kleinschmidt",
  "smoothFanoToricVariety",  
  "orbits",  
  "isDegenerate",
  "isProjective",  
  "isFano",
  "toricDivisor",  
  "ToricDivisor",  
  "isEffective",  
  "isCartier",
  "isQQCartier",
  "isNef",
  "isAmple", 
  "wDiv",    
  "fromWDivToCl",
  "cl",
  "cDiv", 
  "fromCDivToWDiv",   
  "fromCDivToPic",
  "pic",
  "fromPicToCl",
  "nef",
  "emsBound",
  "rawHHOO",
  "makeSimplicial",
  "blowup",
  "makeSmooth"
  }

------------------------------------------------------------------------------
-- CODE
------------------------------------------------------------------------------
KK := QQ  -- global base ring
-- constructing normal toric varieties
NormalToricVariety = new Type of Variety
NormalToricVariety.synonym = "normal toric variety"
NormalToricVariety.GlobalAssignHook = globalAssignFunction
NormalToricVariety.GlobalReleaseHook = globalReleaseFunction
expression NormalToricVariety := X -> new FunctionApplication from { 
  normalToricVariety, Adjacent{rays X, Adjacent{",",max X}}}

normalToricVariety = method(
  TypicalValue => NormalToricVariety, 
  Options => {
    CoefficientRing => KK,
    MinimalGenerators => false,
    Variable => getSymbol "x",	  
    WeilToClass => null})
normalToricVariety (List, List) := opts -> (V,F) -> (
  F' := sort apply(F, s -> sort s);
  X := new NormalToricVariety from {
    symbol rays => V,
    symbol max => F',
    symbol cache => new CacheTable};
  if opts.WeilToClass =!= null then X.cache.fromWDivToCl = opts.WeilToClass;
  X.cache.CoefficientRing = opts.CoefficientRing;
  X.cache.Variable = opts.Variable;
  return X)
normalToricVariety Matrix := opts -> vertices -> (
  if ring vertices =!= ZZ then error "--expected an integer matrix";
  lifting := matrix {toList(numColumns vertices : 1_ZZ)} || vertices;
  H := fourierMotzkin lifting;
  if opts.MinimalGenerators == true then lifting = (fourierMotzkin H)#0;
  V := entries transpose ( -submatrix(H#0,{1..numRows vertices},));
  M := (transpose H#0) * lifting;
  F := apply(numColumns M, j -> select(toList(0..numRows M-1),  
      i -> M_(i,j) == 0));
  return normalToricVariety(V,F,  
    WeilToClass => opts.WeilToClass,
    CoefficientRing => opts.CoefficientRing,
    Variable => opts.Variable))

isWellDefined NormalToricVariety := Boolean => X -> (
  V := rays X;
  F := max X;
  m := #F;
  flag := true;
  dualCones := new MutableHashTable;
  -- check whether every ray appears in some maximal cone
  if set toList(0..#V-1) =!= set flatten F then (
    if debugLevel > 0 then (
      << "-- some ray does not appear in maximal cone" << endl);
    flag = false);
  -- check whether the cones are maximal
  if flag === true then if F =!= unique F or any(F, f -> any(F, 
      g -> all(g, i -> member(i,f)) and g =!=f)) then (
    if debugLevel > 0 then << "-- some cone is not maximal" << endl;
    flag = false);
  -- loop over all maximal cones
  if flag === true then for i to m-1 do (
    f := F#i;
    -- check whether the rays in the cone have the same length
    try C := transpose matrix apply(f, i -> V#i) else (
      if debugLevel > 0 then (
	<< "-- not all rays have the same length" << endl);
      flag = false; 
      break);
    -- check whether the rays are lists of integers
    if ZZ =!= ring C then (
      if debugLevel > 0 then (
	<< "-- not all rays are lists of integers" << endl);
      flag = false; 
      break);
    H := fourierMotzkin C;
    dualCones#i = H#0 | H#1 | -H#1;
    (C',L) := fourierMotzkin H;
    -- check whether the maximal cone is strongly convex
    if L != 0 then (
      if debugLevel > 0 then (
	<< "-- not all maximal cones are strongly convex" << endl);
      flag = false; 
      break);
    -- check whether the rays are the primitive generators of the cone
    if set entries transpose C' =!= set entries transpose C then (
      if debugLevel > 0 then (
	<< "-- the rays are not the primitive generators" << endl);
      flag = false; 
      break);
    if flag === false then break);     
  -- check whether the intersection of each pair of maximal cones is a cone
  if flag === true then for i to m-2 do (
    for j from i+1 to m-1 do (
      C := set apply(toList(set(F#i)*set(F#j)), k -> V#k);	       
      (C',L) := fourierMotzkin (dualCones#i | dualCones#j);
      if C =!= set entries transpose C' then (
	if debugLevel > 0 then (
	  << "-- intersection of cones is not a cone" << endl);
	flag = false; 
	break));
    if flag === false then break);
  return flag)

affineSpace = method(Options => {
    CoefficientRing => KK,
    Variable => getSymbol "x"})
affineSpace ZZ := NormalToricVariety => opts -> d -> (
  if d < 1 then error "-- expected a positive integer";
  return normalToricVariety(entries id_(ZZ^d), {toList(0..d-1)}, 
    CoefficientRing => opts.CoefficientRing, Variable => opts.Variable))

projectiveSpace = method(Options => {
    CoefficientRing => KK,
    Variable => getSymbol "x"})
projectiveSpace ZZ := NormalToricVariety => opts -> d -> (
  if d < 1 then error "-- expected a positive integer";
  V := {toList(d:-1)} | entries id_(ZZ^d);
  F := subsets(d+1,d);
  return normalToricVariety(V,F,
    CoefficientRing => opts.CoefficientRing, Variable => opts.Variable))

weightedProjectiveSpace = method(Options => {
    CoefficientRing => KK,
    Variable => getSymbol "x"})
weightedProjectiveSpace List := NormalToricVariety => opts -> q -> (
  if #q < 2 then error "-- expected a list with at least two elements";
  if not all(q, i -> i > 0) then error "-- expected positive integers";
  d := #q-1;
  if not all(subsets(q,d), s -> gcd s === 1) then (
    error ("--  the " | toString d | "-elements have a common factor"));
  V := entries kernelLLL matrix {q};
  F := subsets(d+1,d);
  return normalToricVariety(V,F,
    CoefficientRing => opts.CoefficientRing, Variable => opts.Variable))

hirzebruchSurface = method(Options => {
    CoefficientRing => KK,
    Variable => getSymbol "x"})
hirzebruchSurface ZZ := NormalToricVariety => opts -> a -> (
  V := {{1,0},{0,1},{-1,a},{0,-1}};
  F := {{0,1},{1,2},{2,3},{0,3}};
  W := matrix{{1,-a,1,0},{0,1,0,1}};
  return normalToricVariety(V,F, 
    CoefficientRing => opts.CoefficientRing, 
    Variable => opts.Variable,
    WeilToClass => W))

NormalToricVariety ** NormalToricVariety := NormalToricVariety => (X,Y) -> (
  V1 := transpose matrix rays X;
  V2 := transpose matrix rays Y;
  V := entries transpose (V1 ++ V2);
  F1 := max X;
  F2 := max Y;
  n := #rays X;
  F2 = apply(F2, s -> apply(s, i -> i+n));
  F := flatten table(F1,F2, (s,t) -> s|t);
  W1 := fromWDivToCl X;
  W2 := fromWDivToCl Y;
  return normalToricVariety(V,F, 
    CoefficientRing => coefficientRing ring X,
    WeilToClass => W1 ++ W2))

NormalToricVariety ^** ZZ := NormalToricVariety => (X,n) -> (
  if n <= 0 then error "expected a positive integer";
  if n == 1 then return X
  else return X ** (X ^** (n-1)))

kleinschmidt = method(Options => {
    CoefficientRing => KK,
    Variable => getSymbol "x"})
kleinschmidt (ZZ,List) := NormalToricVariety => opts -> (d,a) -> (
  if d < 0 then error "-- expected a nonnegative integer";
  r := #a;
  s := d-r+1;
  e := entries id_(ZZ^d);
  if r >= d then error "-- list is too long"; 
  V := {sum(r, i -> -e#i)} | apply(r, i -> e#i);
  V = V | apply(s-1, j -> e#(r+j));
  V = V | {sum(r, i -> a#i*e#i)- sum(s-1, j -> e#(r+j))};
  L := toList(0..r+s);
  F := flatten table(toList(0..r),toList(r+1..r+s), 
    (i,j) -> select(L, k -> i =!= k and j =!= k));
  deg := {{0,1}} | apply(r, i -> {-a#i,1}) | apply(s, j -> {1,0});
  return normalToricVariety(V,F, 
    CoefficientRing => opts.CoefficientRing, 
    Variable => opts.Variable,
    WeilToClass => transpose matrix deg))

-- This function is not exported.
--
-- By reading an auxiliary file, this function creates a HashTable with the
-- defining data for the low dimensional smooth Fano toric varieties.
f4 := currentFileDirectory | "NormalToricVarieties/smoothFanoToricVarieties.txt"
f5 := currentFileDirectory | "NormalToricVarieties/smoothFanoToricVarieties5.txt"
f6 := currentFileDirectory | "NormalToricVarieties/smoothFanoToricVarieties6.txt"
getFano := memoize( d -> (
    local ff;
    if d === 4 then ff = f4
    else if d === 5 then ff = f5
    else if d === 6 then ff = f6;
    if notify then stderr << "--loading file " << ff << endl;
    hashTable apply( lines get ff, x -> (
	x = value x;
	((x#0,x#1),drop(x,2))))))

smoothFanoToricVariety = method(Options => {
    CoefficientRing => KK,
    Variable => getSymbol "x"})
smoothFanoToricVariety (ZZ,ZZ) := NormalToricVariety => opts -> (d,i) -> (
  local s;
  local X;
  if d < 1 or i < 0 then (
    error "-- expected positive dimension or nonnegative index")
  else if d === 1 and i > 0 then (
    error "-- there is only one smooth Fano toric curve")
  else if d === 2 and i > 4 then (
    error "-- there are only five smooth Fano toric surfaces")
  else if d === 3 and i > 17 then (
    error "-- there are only 18 smooth Fano toric 3-folds")
  else if d === 4 and i > 123 then (
    error "-- there are only 124 smooth Fano toric 4-folds")
  else if d === 5 and i > 865 then (
    error "-- there are only 866 smooth Fano toric 5-folds")
  else if d === 6 and i > 7621 then (
    error "-- there are only 7622 smooth Fano toric 6-folds")  
  else if d > 6 then (
    error "-- database doesn't include varieties with dimension > 6")
  else if i === 0 then return projectiveSpace d
  else if d === 5 then (
    s = (getFano(d))#(d,i);
    return normalToricVariety(s#0,s#1,
      CoefficientRing => opts.CoefficientRing, Variable => opts.Variable)) 
  else if d === 6 then (
    s = (getFano(d))#(d,i);
    return normalToricVariety(s#0,s#1,
      CoefficientRing => opts.CoefficientRing, Variable => opts.Variable))
  else (
    s = (getFano(4))#(d,i);
    return normalToricVariety(s#0,s#1, 
      CoefficientRing => opts.CoefficientRing, 
      Variable => opts.Variable,
      WeilToClass => transpose matrix s#2)))

-- this function interfaces with the Polyhedra package
normalToricVariety Fan := opts -> FF -> (
  R := rays FF;
  F := sort apply(maxCones FF, C -> (
      Cr := rays C; 
      Cr = set apply(numColumns Cr, i -> Cr_{i}); 
      positions(R,r -> Cr#?r)));
  R = apply(R, r -> flatten entries r);
  return normalToricVariety(R, F,  
    WeilToClass => opts.WeilToClass,
    CoefficientRing => opts.CoefficientRing,
    Variable => opts.Variable))

normalToricVariety Fan := opts -> FF -> (
--  R := rays FF;
--  F := sort apply(maxCones FF, C -> (
--      Cr := rays C; 
--      Cr = set apply(numColumns Cr, i -> Cr_{i}); 
--      positions(R,r -> Cr#?r)));
--  R = apply(R, r -> flatten entries r);
  return normalToricVariety(entries transpose rays FF, sort maxCones FF,  
    WeilToClass => opts.WeilToClass,
    CoefficientRing => opts.CoefficientRing,
    Variable => opts.Variable))

-- this function interfaces with the Polyhedra package
normalToricVariety Polyhedron := opts -> P -> normalToricVariety normalFan P


------------------------------------------------------------------------------
-- basic properties and invariants

-- The method 'rays' is defined in 'Polyhedra'
rays NormalToricVariety := List => X -> X.rays
max  NormalToricVariety := List => X -> X.max
dim NormalToricVariety := ZZ => (cacheValue symbol dim)(X -> #(rays X)#0)

isDegenerate = method()
isDegenerate NormalToricVariety := Boolean => (cacheValue symbol isDegenerate)(
  X -> kernel matrix rays X != 0)

isSimplicial NormalToricVariety := Boolean => (cacheValue symbol isSimplicial)(
  X -> (
    V := transpose matrix rays X;
    return all(max X, s -> #s == rank V_s)))

isSmooth NormalToricVariety := Boolean => (cacheValue symbol isSmooth)(X -> (
    V := transpose matrix rays X;
    b := all(max X, s -> #s == rank V_s and 1 == minors(#s,V_s));
    if b == true then X.cache.isSimplicial = true;
    return b))

isComplete NormalToricVariety := Boolean => (cacheValue symbol isComplete)(X -> (
    flag := true;
    if orbits(X,1) == {} then flag = false;
    -- there is only one complete normal toric variety of dimension one
    if dim X === 1 then (
      return set rays X === set {{-1},{1}})
    -- check to see that every torus-invariant curve is projective
    else for C in orbits(X,1) when flag === true do (
      m := 0;
      for F in max X when m < 2 do if all(C, i -> member(i,F)) then m = m+1;
      if m < 2 then flag = false);
    return flag))

isProjective = method(TypicalValue => Boolean)
isProjective NormalToricVariety := (cacheValue symbol isProjective)(X -> (
    if not isComplete X then return false
    -- projectivity is easily checked using Gale duality; see Theorem V.4.8 in
    -- Ewald's "Combinatorial convexity and algebraic geometry"
    else (
      n := #rays X;
      B := transpose matrix rays X;
      A := transpose gens kernel B;
      outer := 0 * A_{0};
      for s in max X do (
      	sc := select(n, i -> not member(i,s));
      	outer = outer | (fourierMotzkin A_sc)#0);
      return 0 != (fourierMotzkin outer)#0)))

fan NormalToricVariety := X -> (
  V := promote(matrix transpose rays X, QQ);
  return fan(V, max X))

-- This method is not exported
facesOfCone = method(TypicalValue => HashTable)
-- Given a matrix 'R' whose columns are the rays of a strongly convex cone and
-- a list 's' whose entries label the rays, the method makes a HashTable whose
-- keys label the faces and values give the codimension.
facesOfCone (Matrix,List) := (R,s) -> (
  H := fourierMotzkin R;
  H = H#0 | H#1; 
  incidenceMatrix := (transpose H) * R;
  h := numColumns H;  
  hyperplaneTable := new MutableHashTable from apply(h, i -> {{i}, select(s, 
      	j -> incidenceMatrix_(i, position(s, l -> l === j)) === 0)});
  faceTable := new MutableHashTable from apply(values hyperplaneTable, f -> {f,1});
  faceTable#s = 0;
  d := rank R;
  Q := apply(h, i -> {i});
  while Q =!= {} do (
    q := first Q;
    Q = drop(Q,1);
    for i from 0 to h-1 do if not member(i,q) then (
      t := select(hyperplaneTable#q, j -> member(j,hyperplaneTable#{i}));
      k := sort(q | {i});
      if t =!= {} and not hyperplaneTable#?k and not faceTable#?t then (
	hyperplaneTable#k = t;
	faceTable#t = d - rank R_(positions(s, i -> member(i,t)));
	Q = Q | {k})));
  d = numRows R - d;
  return new HashTable from apply(keys faceTable, f -> {f,d+faceTable#f}))
-- Given a list 'L' whose entries label rays in a simplicial cone and an
-- integer 'i' which is the codimension of the cone, this method makes a
-- HashTable whose keys label the faces and values give the codimension, In
-- the simplicial case, we don't actually need the rays of the cone.
facesOfCone (List,ZZ) := (L,i) -> (
  new HashTable from apply(drop(subsets(L),1), s -> {s,#L-#s+i}))

orbits = method()   
orbits NormalToricVariety := HashTable => (cacheValue symbol orbits)(X -> (
    H := new HashTable;
    R := transpose matrix rays X; 
    d := dim X;
    if isSimplicial X and not isDegenerate X then (
      for s in max X do H = merge(H,facesOfCone(s, d - rank R_s), (p,q) -> p))
    else for s in max X do H = merge(H,facesOfCone(R_s,s), (p,q) -> p);
    O := new MutableHashTable from apply(d, i -> {i,{}});
    for k in keys H do O#(H#k) = O#(H#k) | {k};
    return new HashTable from apply(keys O, k -> {k, sort O#k}) | {{d,{}}} ))
orbits (NormalToricVariety, ZZ) := List => (X,i) -> (
  if i < 0 or i > dim X then (
    error "-- expected a nonnegative integer that is at most the dimension");
  O := orbits X;
  O#i)

------------------------------------------------------------------------------
-- divisor, class, and Picard groups
cl = method()
cl NormalToricVariety := Module => (cacheValue symbol cl)(X -> (
    rawC := cokernel matrix rays X;
    C := prune rawC;
    -- We also compute the map to the group of Weil divisors
    W := wDiv X;
    local A;
    if X.cache.?fromWDivToCl then A = matrix X.cache.fromWDivToCl
    else A = map(C, W, matrix (C.cache.pruningMap)^-1);
    X.cache.fromWDivToCl = map(C,W,A);	  
    return C))

fromWDivToCl = method()
fromWDivToCl NormalToricVariety := Matrix => X -> (
  if not X.cache.?cl then cl X;
  return X.cache.fromWDivToCl)

wDiv = method(TypicalValue => Module)
wDiv NormalToricVariety := (cacheValue symbol wDiv)(X -> ZZ^(#rays X))

cDiv = method(TypicalValue => Module)
cDiv NormalToricVariety := (cacheValue symbol cDiv)(X -> (
    local CDiv;
    if isSmooth X then (
      CDiv = wDiv X;
      X.cache.fromCDivToWDiv = id_CDiv;
      return CDiv)
    else (
      V := transpose matrix rays X;
      F := max X;
      d := dim X;
      n := #rays X;
      H1 := new HashTable from apply(F, 
	s -> {s, coker (fourierMotzkin V_s)#1});
      H2 := new HashTable from (
	flatten apply(toList(0..#F-1),  i -> apply(toList(i+1..#F-1), j -> (
	      s := select(F#i, k -> member(k,F#j));
	      if #s > 0 then (
		M := coker (fourierMotzkin V_s)#1;
		{(F#i,F#j), M})))));
      K := keys H1;
      P1 := directSum apply(K, k -> k => H1#k);
      local D;
      if #keys H2 == 0 then D = ker map(ZZ^0,P1,0) 
      else (
	P2 := directSum apply(keys H2, k -> k => H2#k);
	M := transpose matrix table(K, keys H2, (j,k) -> if j == k#0 then 1 
	  else if j == k#1 then -1 else 0);
	D = kernel map(P2,P1,M ** id_(ZZ^d)));
      CDiv = prune D;
      L := apply(n, i -> position(K, s -> member(i,s)));
      inc := matrix table(n,keys H1, (i,s) -> if s == K#(L#i) then 1 else 0);
      -- We also compute the map to the group of Weil divisors
      local iota;
      iota = inc^{0} ** transpose V_{0};
      scan(#L - 1, i -> iota = iota || inc^{i+1} ** transpose V_{i+1});
      iota = map(wDiv X, D, iota * gens D);
      eta := CDiv.cache.pruningMap;
      X.cache.fromCDivToWDiv = map(wDiv X, CDiv, iota * eta);
      --output	       
      return CDiv)))

fromCDivToWDiv = method()
fromCDivToWDiv NormalToricVariety := Matrix => X -> (
  if not X.cache.?fromCDivToWDiv then cDiv X;
  return X.cache.fromCDivToWDiv)

pic = method()
pic NormalToricVariety := Module => (cacheValue symbol pic)(X -> (
    local C;
    if isSmooth X then (
      C = cl X;
      X.cache.fromPicToCl = id_C;
      X.cache.fromCDivToPic = fromWDivToCl X;
      return C)
    else (
      V := rays X;
      d := dim X;
      phi := map(wDiv X, ZZ^d, matrix V);
      psi := fromCDivToWDiv X;
      rawP := subquotient(psi,phi);
      P := prune rawP;
      iota := P.cache.pruningMap;
      X.cache.fromCDivToPic = map(P,cDiv X, iota^-1);
      C = cokernel matrix rays X;
      theta := inducedMap(C,rawP);
      eta := map(cl X, C, matrix fromWDivToCl X);
      X.cache.fromPicToCl = eta * theta * iota;
      return P)))

fromPicToCl = method()
fromPicToCl NormalToricVariety := Matrix => X -> (
  if not X.cache.?fromPicToCl then pic X;
  return X.cache.fromPicToCl)

fromCDivToPic = method()
fromCDivToPic NormalToricVariety := Matrix => X -> (
  if not X.cache.?fromCDivToPic then pic X;
  return X.cache.fromCDivToPic)
     
nef = method()
nef NormalToricVariety := List => X -> (
  if not isComplete X then return false
  else (
    n := #rays X;
    A := transpose matrix degrees ring X;
    outer := 0 * A_{0};
    for s in max X do (
      sc := select(n, i -> not member(i,s));
      outer = outer | (fourierMotzkin A_sc)#0);
    return transpose ((fourierMotzkin outer)#0)^{0..(n-dim X-1)}))


------------------------------------------------------------------------------
-- toric divisors
ToricDivisor = new Type of HashTable
ToricDivisor.synonym = "toric divisor"

expression ToricDivisor := D -> (
  S := support D;
  local E;
  local E';
  local j;
  negative := false;
  if S === {} then return expression 0
  else (
    j = first S;
    c := D#j;
    if c < 0 then negative = true;
    c = abs(c);
    if c === 1 then E = Subscript{"D", j} 
    else E = Product{c, Subscript{"D", j}};
    if negative === true then (
      negative = false;
      E = Minus E);
    for i from 1 to #S-1 do (
      j = S#i;
      c = D#(S#i);
      if c < 0 then negative = true;
      c = abs(c);
      if c === 1 then E' = Subscript{"D", j} 
      else E' = Product{c, Subscript{"D", j}};
      if negative === true then (
	negative = false;
	E' = Minus E');
      E = E + E');
    return E))
net ToricDivisor := D -> net expression D
ToricDivisor#{Standard,AfterPrint} = ToricDivisor#{Standard,AfterNoPrint} = D ->(
  << endl;				  -- double space
  << concatenate(interpreterDepth:"o") << lineNumber << " : ToricDivisor on ";
  << variety D << endl;)

normalToricVariety ToricDivisor := NormalToricVariety => opts -> D -> D.variety
variety ToricDivisor := NormalToricVariety => D -> normalToricVariety D
support ToricDivisor := List => D -> (
  select(toList(0..D.number-1), i -> D#i =!= 0))

toricDivisor = method(TypicalValue => ToricDivisor)
toricDivisor (List,NormalToricVariety) := (L,X) -> (
  n := #L;
  if n =!= #rays X then (
    error "-- number of elements in the list is not equal to the number rays");
  H := {{symbol variety, X},{symbol number, #L},{symbol cache, new CacheTable}};
  H = H | apply(#L, i -> {i,L#i});
  new ToricDivisor from H)

NormalToricVariety _ ZZ := ToricDivisor => (X,i) -> (
  n := #rays X;
  if i < 0 or i > n-1 then (
    error "-- expect the integer to index a ray of normal toric variety");
  L := apply(n, j -> if j === i then 1_ZZ else 0_ZZ);
  return toricDivisor(L,X))

toricDivisor NormalToricVariety := ToricDivisor => X -> (
  sum(#rays X, i -> -X_i))

ToricDivisor + ToricDivisor := ToricDivisor => (D,E) -> (
  X := variety D;
  if X =!= variety E then error "-- expected divisors on the same variety";
  return toricDivisor(apply(D.number, i -> D#i+E#i),X))
ToricDivisor - ToricDivisor := ToricDivisor => (D,E) -> (
  X := variety D;
  if X =!= variety E then error "-- expected divisors on the same variety";
  return toricDivisor(apply(D.number, i -> D#i-E#i),X))
ZZ * ToricDivisor := ToricDivisor => (n,D) -> (
  return toricDivisor(apply(D.number, i -> n*D#i), variety D))
- ToricDivisor := ToricDivisor => D -> (-1)*D

vector ToricDivisor := Vector => D -> (
  vector first entries matrix{apply(D.number, i -> D#i)})

installMethod(symbol SPACE, OO, ToricDivisor, (OO, D) -> (
  X := variety D;
  a := toSequence entries (fromWDivToCl X * vector D);
  return OO_X a))

isEffective = method(TypicalValue => Boolean);
isEffective ToricDivisor := D -> all(D.number, i -> D#i >= 0)

isCartier = method(TypicalValue => Boolean);
isCartier ToricDivisor := D -> matrix vector D % fromCDivToWDiv variety D == 0

isQQCartier = method(TypicalValue => Boolean)
isQQCartier ToricDivisor := D -> (
  X := variety D;
  V := (matrix rays X) ** QQ;
  a := (matrix vector D) ** QQ;
  F := max X;
  m := apply(F, s -> a^s // V^s);
  return all(apply(#F, i -> a^(F#i) - V^(F#i)*m#i), j -> j == 0))

-- This method is not exported.
--
-- Given a toric divisor which is assumed to be Cartier, this method
-- characters on each maximal cone which determine the Cartier divisor.
cartierCoefficients = method()
cartierCoefficients ToricDivisor := List => D -> (
  X := variety D;
  V := matrix rays X;
  a := matrix vector D;
  return apply(max X, s -> a^s // V^s))

isNef = method(TypicalValue => Boolean)
isNef ToricDivisor := D -> (
  X := variety D;
  if not isComplete X or not isQQCartier D then return false
  -- the unique complete toric variety of dimension one is the projective line
  -- which we treat as a simply special case
  else if dim X === 1 then (
    return sum entries vector D >= 0
    )
  -- a torus-invariant divisor is nef if and only if the interesection with
  -- every torus-invariant curve is nonnegative
  else (
    m := cartierCoefficients D;
    F := max X;
    V := matrix rays X;
    return all(orbits(X,1), c -> (
	(p,q) := toSequence select(#F, i -> all(c, j -> member(j,F#i)));
	k := position(F#q, i -> not member(i,c));
	v := promote(V^{F#q#k} * (m#q-m#p),QQ);
	N := prune coker transpose (V^c ** QQ);
	u := transpose matrix (N.cache.pruningMap)^(-1);
	w := promote(V^{F#q#k},QQ) * u;
	if w_(0,0) < 0 then w = (-1)* w;
	(v // w)_(0,0) >= 0))))

isAmple = method(TypicalValue => Boolean)
isAmple ToricDivisor := D -> (
  X := variety D;
  if not isComplete X or not isCartier D then return false
  -- the unique complete toric variety of dimension one is the projective line
  -- which we treat as a simply special case
  else if dim X === 1 then (
    return sum entries vector D > 0
    )
  -- the "toric Kleiman criterion" states that a torus-invariant divisor is
  -- ample if and only if the interesection with every torus-invariant curve
  -- is positive
  else (
    m := cartierCoefficients D;
    F := max X;
    V := matrix rays X;
    return all(orbits(X,1), c -> (
	(p,q) := toSequence select(#F, i -> all(c, j -> member(j,F#i)));
	k := position(F#q, i -> not member(i,c));
	v := promote(V^{F#q#k} * (m#q-m#p),QQ);
	N := prune coker transpose (V^c ** QQ);
	u := transpose matrix (N.cache.pruningMap)^(-1);
	w := promote(V^{F#q#k},QQ) * u;
	if w_(0,0) < 0 then w = (-1)* w;
	(v // w)_(0,0) > 0))))

hilbertBasis(Matrix,Thing) := Matrix => opts -> (C,notused) -> (
    transpose (normaliz(transpose C,"integral_closure"))#"gen")

isVeryAmple ToricDivisor := Boolean => D -> (
  if not isAmple D then return false
  else if isSmooth variety D then return true
  else (
    V := vertices D;
    n := numColumns V;
    d := numRows V;
    L := latticePoints D;
    m := numColumns L;
    return all(n, i -> (
	H := hilbertBasis(V - matrix {toList(n:1)} ** V_{i}, "notused");
	P := L - matrix {toList(m:1)} ** V_{i};
	isSubset(set entries transpose H, set entries transpose P)))))
     
isFano = method(TypicalValue => Boolean)
isFano NormalToricVariety := X -> isAmple (- toricDivisor X)

vertices ToricDivisor := Matrix => D -> (
  if not isCartier D then error "-- expected a Cartier divisor";
  X := variety D;
  if not isComplete X then error "-- expected a divisor on a complete toric variety";
  if not isEffective D then return null
  else (
    d := dim X;
    V := transpose (matrix vector D | matrix rays variety D);
    V = V | transpose matrix {{1} | toList(d:0)};
    return -((fourierMotzkin V)#0)^{1..d}))

latticePoints ToricDivisor := Matrix => D -> (
  V := vertices D;
  if V === null then return null
  else (
    d := numRows V;
    V = transpose (normaliz(transpose V,"polytope"))#"gen";
    s := select(numColumns V, i -> V_(d,i) === 1);
    c := (V_s)^{0..d-1};
    return c_(sortColumns c)))

polytope ToricDivisor := Polyhedron => (cacheValue symbol polytope)(
  D -> intersection(-matrix rays variety D, matrix vector D))

------------------------------------------------------------------------------
-- Total coordinate rings
ring NormalToricVariety := PolynomialRing => (cacheValue symbol ring)(X -> (
    if isDegenerate X then (
      error "-- not yet implemented for degenerate varieties");
    if not isFreeModule cl X then (
      error "-- gradings by torsion groups not yet implemented");
    -- constructing ring
    K := X.cache.CoefficientRing;	  
    x := X.cache.Variable;	  
    n := #rays X;
    deg := entries transpose matrix fromWDivToCl X;
    S := K(monoid[x_0..x_(n-1), Degrees => deg]);
    S.variety = X;
    return S))

variety Ring := Variety => S -> if S.?variety then S.variety else null
normalToricVariety Ring := NormalToricVariety => opts -> S -> variety S

ideal NormalToricVariety := Ideal => (cacheValue symbol ideal)(X -> (
    S := ring X;
    n := numgens S;
    return ideal apply(max X, 
      L -> product(n, i -> if member(i,L) then 1_S else S_i))))
monomialIdeal NormalToricVariety := MonomialIdeal => X -> monomialIdeal ideal X

sheaf (NormalToricVariety,Module) := CoherentSheaf => (X,M) -> (
  if ring M =!= ring X then (
    error "-- expected module and variety to have the same ring");
  if not isHomogeneous M then (
    error "-- expected a homogeneous module");
  -- constructing coherent sheaf
  new CoherentSheaf from {
    symbol module => M,
    symbol variety => X})

sheaf (NormalToricVariety,Ring) := SheafOfRings => (X,R) -> (
    if ring X =!= R then error "-- expected the ring of the variety";
    return new SheafOfRings from { 
      symbol variety => X, 
      symbol ring    => R })

sheaf NormalToricVariety := X -> sheaf_X ring X

CoherentSheaf#{Standard,AfterPrint} = F -> (
     X := variety F;
     M := module F;
     << endl;				  -- double space
     n := rank ambient F;
     << concatenate(interpreterDepth:"o") << lineNumber << " : coherent sheaf on " << X;
     if M.?generators then
     if M.?relations then << ", subquotient of " << ambient F
     else << ", subsheaf of " << ambient F
     else if M.?relations then << ", quotient of " << ambient F;
     << endl;)

installMethod(symbol _, OO, NormalToricVariety, (OO,X) -> sheaf(X, ring X))

CoherentSheaf Sequence := CoherentSheaf => (F,a) -> sheaf(variety F, 
     F.module ** (ring F)^{toList(a)})
   
sheafHom(CoherentSheaf,CoherentSheaf) := (F,G) -> (
  sheaf(variety F, Hom(module F, module G)))
Hom(CoherentSheaf,CoherentSheaf) := Module => (F,G) -> HH^0(variety F, sheafHom(F,G))

SheafOfRings Sequence := CoherentSheaf => (O,a) -> O^1 a

super   CoherentSheaf := CoherentSheaf => F -> sheaf(variety F, super   module F)
ambient CoherentSheaf := CoherentSheaf => F -> sheaf(variety F, ambient module F)
cover   CoherentSheaf := CoherentSheaf => F -> sheaf(variety F, cover   module F)

minimalPresentation CoherentSheaf := prune CoherentSheaf := opts -> F -> (
  X := variety F;
  if class X === NormalToricVariety then (
    M := module F;
    S := ring M;
    B := ideal X;
    N := saturate(image map(M,S^0,0),B);
    if N != 0 then M = M/N;
    C := res M;
    -- is there a better bound?
    a := max(1, max flatten flatten apply(length C +1, i -> degrees C#i));
    return sheaf(X, minimalPresentation Hom(B^[a], M)))
  else return sheaf minimalPresentation HH^0 F(>=0))

cotangentSheaf NormalToricVariety := CoherentSheaf => opts -> (
  (cacheValue (symbol cotangentSheaf => opts))(X -> (
      if isDegenerate X then error "-- expect a non-degenerate toric variety";
      S := ring X;
      d := dim X;
      n := numgens S;
      nu := map(S^n, S^d, (matrix rays X) ** S);
      eta := map(directSum apply(n, i -> S^1/ideal(S_i)), S^n, id_(S^n));
      om := sheaf(X, kernel (eta * nu));
      if opts.Minimize then om = minimalPresentation om;
      return om)))

cotangentSheaf(ZZ,NormalToricVariety) := CoherentSheaf => opts -> (i,X) -> (
  return exteriorPower(i, cotangentSheaf(X,opts)))

-- This function is not exported.
--
-- Given a normal toric variety, this function creates a HashTable describing
-- the cohomology of all twists of the structure sheaf.  For more information,
-- see Propositon~3.2 in Maclagan-Smith "Multigraded regularity"
setupHHOO = X -> (
  X.cache.emsBound = new MutableHashTable;
  -- create a fine graded version of the total coordinate ring
  S := ring X;
  n := numgens S;
  fineDeg := entries id_(ZZ^n);
  h := toList(n:1);
  R := QQ(monoid [gens S, Degrees => fineDeg, Heft => h]);
  RfromS := map(R, S, gens R);
  B := RfromS ideal X;
  -- use simplicial cohomology find the support sets 
  quasiCech := Hom(res(R^1/B), R^1);
  supSets := delete({},subsets(toList(0..n-1)));
  d := dim X;
  sigma := new MutableHashTable;
  sigma#0 = {{{},1}};
  for i from 1 to d do (
    sHH := prune HH^(i+1)(quasiCech);
    sigma#i = for s in supSets list (
      m := product(s, j ->  R_j);
      b := rank source basis(-degree m, sHH);
      if b > 0 then {s,b} else continue));
  -- create rings
  degS := degrees S; 
  X.cache.rawHHOO = new HashTable from apply(d+1, 
    i -> {i, apply(sigma#i, s -> (
	  v := - degree product(n, 
	    j -> if member(j,s#0) then S_j else 1_S);
	  degT := apply(n, 
	    j -> if member(j,s#0) then -degS#j else degS#j);
	  T := (ZZ/2)(monoid [gens S, Degrees => degT]);
	  {v,T,s#0,s#1}))}))

-- Defines the Frobenius power of an ideal
Ideal ^ Array := (I,p) -> ideal apply(I_*, i -> i^(p#0))

-- This function is not exported.
--
-- This function creates a HastTable which stores the data for determining the
-- appropriate Frobenius power needed to compute the cohomology of a general
-- coherent sheaf; see Proposition 4.1 in Eisenbud-Mustata-Stillman.
emsbound = (i,X,deg) -> (
  if not X.cache.emsBound#?{i,deg} then (
    if i < 0 or i > dim X then X.cache.emsBound#{i,deg} = 1
    else X.cache.emsBound#{i,deg} = max ( {1} | apply(X.cache.rawHHOO#i, 
	t -> #t#2 + max apply(first entries basis(deg-t#0,t#1),
	  m -> max (first exponents m)_(t#2)))));
  X.cache.emsBound#{i,deg})

cohomology (ZZ,NormalToricVariety,CoherentSheaf):= Module => opts -> (i,X,F) -> (
  if ring F =!= ring X then (
    error "-- expected a coherent sheaf on the toric variety");
  S := ring X;
  kk := coefficientRing S;
  if not isField kk then error "-- expected a toric variety over a field";
  if i < 0 or i > dim X then kk^0
  else (
    if not X.cache.?rawHHOO then setupHHOO X;
    M := module F;
    if isFreeModule M then kk^(
      sum apply(degrees M, deg -> sum apply(X.cache.rawHHOO#i,
	  t -> rank source basis(-deg-t#0,(t#1)^(t#3)))))
    else (
      B := ideal X;
      C := res M;
      deg := toList(degreeLength S : 0);
      bettiNum := flatten apply(1+length C, 
	j -> apply(unique degrees C_j, alpha -> {j,alpha}));
      b1 := max apply(bettiNum, 
	beta -> emsbound(i+beta#0-1,X,deg-beta#1));
      b2 := max apply(bettiNum, 
	beta -> emsbound(i+beta#0,X,deg-beta#1));	       
      b := max(b1,b2);
      if i > 0 then kk^(rank source basis(deg, Ext^(i+1)(S^1/B^[b],M)))
      else (
	h1 := rank source basis(deg, Ext^(i+1)(S^1/B^[b],M));
	h0 := rank source basis(deg, Ext^i(S^1/B^[b1],M));
	kk^(rank source basis(deg,M) + h1 - h0)))))

cohomology (ZZ,NormalToricVariety,SheafOfRings) := Module => opts -> (
  (i,X,O) -> HH^i(X,O^1))

euler CoherentSheaf := F -> (
  X := variety F;
  if class variety F === NormalToricVariety then 
  return sum(1 + dim X, i -> (-1)^i*(rank HH^i(X,F)))
  else if class variety F === ProjectiveVariety then
  return euler module F
  else error "expected a sheaf on a ProjectiveVariety or NormalToricVariety")


------------------------------------------------------------------------------
-- resolution of singularities

-- This method is not exported.
--
-- Given a normal toric variety 'X', a maximal cone indexed by the list 's',
-- and a weight vector encoded by the integer list 'w', this method makes a
-- new normal toric variety in which the maximal cone corresponding to 's' has
-- been replace by the regular subdivison associated to the weight vector 'w'.
-- In particular, the entries in 'w' are used as heights to lift the maximal
-- cone corresponding to 's' into the next dimension.  The lower faces (those
-- for which the normal vector has negative last coordinate) form a polyhedral
-- complex and the regular subdivision is the image of this complex.  For a
-- generic weight vector, this subdivision will be a triangulation.
regularSubdivision = method(TypicalValue => NormalToricVariety)
regularSubdivision (NormalToricVariety, List, List) := (X,s,w) -> (
    F := max X;
    R := rays X;
    V := transpose matrix R;
    wtg := i -> if member(i,s) then w#(position(s, j -> i === j)) else 1;    
    for f in F do (
      if #f === rank V_f then continue;
      w' := f / wtg;
      if all(w', i -> i === 1) then continue;
      C := V_f || matrix{w'};
      C' := fourierMotzkin C;
      H := matrix select(entries transpose C'#0, r -> last r < 0);
      if C'#1 !=0 then (  
  	H' := select(entries transpose C'#1, r -> last r != 0);
  	if #H' > 0 then H = H || matrix apply(H', 
	  r -> if last r > 0 then -r else r));
      inc := H * C;
      F' := apply(apply(numRows inc, i -> select(numColumns inc,
	    j -> inc_(i,j) === 0)), t -> f_t);
      k := position(F, t -> t === f);
      if all(F', t -> #t == rank V_f) then F = drop(F,{k,k}) | F');
    if F == max X then return X
    else (
      Y := normalToricVariety(R,F);
      Y.cache.Weights = apply(#R, i -> wtg i));
    return Y)    

makeSimplicial = method(
  TypicalValue => NormalToricVariety,
  Options => {Strategy => 0})
makeSimplicial NormalToricVariety := opts -> X -> (
  Y := X;
  local F;
  local V;
  local k;
  local s;
  if opts.Strategy === 1 then (
    while true do (
      F = max Y;
      V = transpose matrix rays Y;
      k = position(F, t -> #t =!= rank V_t);
      if k === null then break
      else (
      	s = F#k;
      	c := 1 + dim Y - rank V_s;
      	i := 0;
      	edges := select(orbits(Y,c), r -> all(r, j -> member(j,s)));
      	while #select(edges, r -> not member(s#i,r)) === 1 do i = i+1;
      	Y = blowup({s#i},Y))))
  else while true do (
    F = max Y;
    V = transpose matrix rays Y;
    k = position(F, t -> #t =!= rank V_t);
    if k === null then break
    else (
      s = F#k;
      n := #s;
      m := (n // 10) + 1;
      w := apply(n, i -> random(2,100*m));
      Y = regularSubdivision(Y,s,w)));
  return Y)

-- This method is not exported.
makePrimitive = method()
-- Given a list 'w' of integers, this method returns the associated primitive
-- vector; it divides the entries by their greatest common denominator
makePrimitive List := List => w -> (
  g := gcd w;
  if g > 1 then return apply(w, i -> i // g)
  else return w)

blowup = method()
blowup (List, NormalToricVariety, List) := NormalToricVariety => (s,X,v) -> (
  F := max X;
  starIndex := positions(F, t -> all(s, i -> member(i,t)));
  star := F_starIndex;
  V := transpose matrix rays X;
  d := dim X;
  clStar := {};
  for t in star do (
    c := 1 + d - rank V_t;
    clStar = clStar | select(orbits(X,c), r -> all(r, j -> member(j,t))));
  clStar = unique clStar;
  n := #rays X;
  F = F_(select(#F, i -> not member(i, starIndex)));
  local F';
  if #s === 1 then (
    F' = for t in clStar list (
      if member(s#0,t) then continue
      else sort(t | s));
    return normalToricVariety(rays X, F | F'))
  else (
    F' = for t in clStar list (
      if all(s, i -> member(i,t)) then continue
      else t | {n});
  return normalToricVariety(rays X | {v}, F | F')))

blowup (List, NormalToricVariety) := NormalToricVariety => (s,X) -> (
  v := makePrimitive sum ((rays X)_s);
  return blowup(s,X,v))

makeSmooth = method(
  TypicalValue => NormalToricVariety,
  Options => {Strategy => 0})
makeSmooth NormalToricVariety := opts -> X -> (
  Y := X;
  while true do (
    F := max Y;
    V := transpose matrix rays Y;
    k := position(F, t -> #t =!= rank V_t or 1 != minors(#t,V_t));
    if k === null then break
    else (
      sigma := F#k;
      tau := first select(select(subsets(sigma), t -> #t > 1), 
  	f -> #f =!= rank V_f or 1 != minors(#f,V_f));
      Vt := V_tau;
      H := hilbertBasis(posHull Vt);
      H = H/(v -> flatten entries v);
      --time H := entries transpose hilbertBasis(Vt,"notused");
      w := select(H, h -> not member(h, (rays Y)_sigma));
      if w === {} then Y = makeSimplicial(Y, Strategy => opts.Strategy)
      else Y = blowup(tau,Y, first w)));
  return Y)

------------------------------------------------------------------------------
-- THINGS TO IMPLEMENT?
--   homology,NormalToricVariety
--   operational Chow rings
--   linear series
--   isSemiprojective
--   toric maps
--     pullback divisors
--     pushforward divisors
--     birational map with makeSimplicial & makeSmooth
--

------------------------------------------------------------------------------
-- DOCUMENTATION
------------------------------------------------------------------------------
beginDocumentation()
    
document { 
  Key => NormalToricVarieties,
  Headline => "normal toric varieties",
  "A toric variety is an integral scheme such that an algebraic torus forms a
  Zariski open subscheme and the natural action this torus on itself extends 
  to an action on the entire scheme.  Normal toric varieties correspond to
  combinatorial objects, namely strongly convex rational polyhedral fans.  
  This makes the theory of normal toric varieties very explicit and 
  computable.",
  PARA{},     
  "This ", EM "Macaulay2", " package is designed to manipulate normal toric
  varieties and related geometric objects.  An introduction to the theory of
  normal toric varieties can be found in the following textbooks:",
  UL { 
    {"David A. Cox, John B. Little, Hal Schenck, ", 
      HREF("http://www.cs.amherst.edu/~dac/toric.html", EM "Toric varieties"),
      ", Graduate Studies in Mathematics 124. American Mathematical Society, 
      Providence RI, 2011.  ISBN: 978-0-8218-4817-7"},
    {"Günter Ewald, ", EM "Combinatorial convexity and algebraic geometry", ",
      Graduate Texts in Mathematics 168.  Springer-Verlag, New York, 1996. 
      ISBN: 0-387-94755-8" },      
    {"William Fulton, ", EM "Introduction to toric varieties", ", Annals of
      Mathematics Studies 131, Princeton University Press, Princeton, NJ,
      1993. ISBN: 0-691-00049-2" },	
    {"Tadao Oda, ", EM "Convex bodies and algebraic geometry, an introduction 
      to the theory of toric varieties", ", Ergebnisse der Mathematik und 
      ihrer Grenzgebiete (3) 15, Springer-Verlag, Berlin, 1988. ISBN: 
      3-540-17600-4" },
     },
  SUBSECTION "Contributors",
  "The following people have generously contributed code or worked on our 
  code.",
  UL {
    {HREF("http://www.math.duke.edu/~psa/","Paul Aspinwall")},
    {HREF("http://www2.math.su.se/~cberkesc/","Christine Berkesch")},
    {HREF("http://page.mi.fu-berlin.de/rbirkner/indexen.htm","René Birkner")},
    {HREF("http://www.warwick.ac.uk/staff/D.Maclagan/","Diane Maclagan")},
    {HREF("http://www.math.uiuc.edu/~asecele2/","Alexandra Seceleanu")},},
  SUBSECTION "Menu",
  UL {
    TO "Making normal toric varieties",
    TO "Basic invariants and properties of normal toric varieties",
    TO "Working with divisors and their associated groups",
    TO "Total coordinate rings and coherent sheaves",
    TO "Resolution of singularities"}}

document { 
  Key => "Making normal toric varieties",
  "A normal toric variety corresponds to a strongly convex rational polyhedral
  fan in affine space.  ", "In this package, the fan associated to a normal ",
  TEX ///$d$///, "-dimensional toric variety lies in the rational vector space
  ", TEX ///$\QQ^d$///, " with underlying lattice ", TEX ///$N = \ZZ^d$///, ".
  The fan is encoded by the minimal nonzero lattice points on its rays and the
  set of rays defining the maximal cones (a maximal cone is not properly
  contained in another cone in the fan).",
  PARA{},
  "The general method for creating a normal toric variety is ", 
  TO normalToricVariety, ".  However, there are many additional methods for
  constructing other specific types of normal toric varieties.",
  SUBSECTION "Menu",
  UL {
    TO (normalToricVariety,List,List),
    TO (normalToricVariety,Matrix),
    TO NormalToricVariety,
    TO (isWellDefined, NormalToricVariety),
    TO (affineSpace,ZZ),
    TO (projectiveSpace,ZZ),
    TO (weightedProjectiveSpace,List),
    TO (hirzebruchSurface,ZZ),
    TO (kleinschmidt,ZZ,List),
    TO (symbol **, NormalToricVariety, NormalToricVariety),
    TO (symbol ^**, NormalToricVariety, ZZ),
    TO (smoothFanoToricVariety,ZZ,ZZ),
    TO (normalToricVariety, Fan),
    TO (normalToricVariety, Polyhedron)},
  "Several methods for making new normal toric varieties from old ones are
  listed in the section on resolution of singularities.",
  SeeAlso =>{
    "Basic invariants and properties of normal toric varieties",
    "Working with divisors and their associated groups",
    "Total coordinate rings and coherent sheaves",
    "Resolution of singularities"}}

document { 
  Key => {NormalToricVariety},
  Headline => "the class of all normal toric varieties",  
  "A normal toric variety corresponds to a strongly convex rational polyhedral
  fan in affine space.  ", "In this package, the fan associated to a normal ",
  TEX ///$d$///, "-dimensional toric variety lies in the rational vector space
  ", TEX ///$\QQ^d$///, " with underlying lattice ", TEX ///$N = \ZZ^d$///, ".
  The fan is encoded by the minimal nonzero lattice points on its rays and the
  set of rays defining the maximal cones (a maximal cone is not properly
  contained in another cone in the fan).",
  Caveat => {"By assumption, all normal toric varieties in this package have
             positive dimension."},
  SeeAlso => {
    "Making normal toric varieties",
    normalToricVariety,
    (rays,NormalToricVariety),
    (max,NormalToricVariety),
    (expression,NormalToricVariety)}}

document { 
  Key => {normalToricVariety, 
    (normalToricVariety,List,List), 
    [normalToricVariety,CoefficientRing],
    [normalToricVariety,Variable],
    [normalToricVariety,WeilToClass],
    WeilToClass},
  Headline => "make a normal toric variety",
  Usage => "normalToricVariety(Rho,Sigma)",
  Inputs => {
    "Rho" => List => "of lists of integers; each entry is the minimal nonzero
                     lattice point on a ray in the fan",
    "Sigma" => List => "of lists of nonnegative integers; each entry indexes the
                       rays defining a maximal cone in the fan",
    CoefficientRing => Ring => "the coefficient ring of the total coordinate 
                               ring",
    MinimalGenerators => Boolean => "an option which specifics whether to
                                    compute minimal generators",
    Variable => Symbol => "the base symbol for the indexed variables in the
                          total coordinate ring",
    WeilToClass => Matrix => {"allows one to specify the map from the group of
                              torus-invariant Weil divisors to the class 
			      group"},
    },
  Outputs => {NormalToricVariety => "the normal toric variety determined by 
                                     the fan" },
  "This is the general method for constructing a normal toric variety.",
  PARA{},				    
  "A normal toric variety corresponds to a strongly convex rational polyhedral
  fan in affine space.  In this package, the fan associated to a normal ", 
  TEX ///$d$///, "-dimensional toric variety lies in the rational vector 
  space ", TEX ///$\QQ^d$///, " with underlying lattice ", 
  TEX ///$N = \ZZ^d$///, ".  The fan is encoded by the minimal nonzero lattice
   points on its rays and the set of rays defining the maximal cones (meaning 
  cones that are not proper subsets of another cone in the fan).  More 
  precisely, ", TT "Rho", " lists the minimal nonzero lattice points on each 
  ray (a.k.a. one-dimensional cone) in the fan. Each lattice point is a ", 
  TO2(List,"list"), " of ", TO2(ZZ,"integers"), ". The rays are ordered and 
  indexed by nonnegative integers: ", TEX ///$0,\dots,n$///, ".  Using this 
  indexing, a maximal cone in the fan corresponds to a sublist of ", 
  TEX ///$\{0,\dots,n\}$///, ".  All maximal cones are listed in ", 
  TT "Sigma", ".",
  PARA{},
  "The first example is projective ", TEX ///$2$///, "-space blown up at two
  points.",
  EXAMPLE lines ///
    Rho = {{1,0},{0,1},{-1,1},{-1,0},{0,-1}}
    Sigma = {{0,1},{1,2},{2,3},{3,4},{0,4}}
    X = normalToricVariety(Rho,Sigma)
    rays X
    max X
    dim X
    ///,	 
  "The second example illustrates the data defining projective ", 
  TEX ///$4$///, "-space.",
  EXAMPLE lines ///	  
    PP4 = projectiveSpace 4;
    rays PP4
    max PP4
    dim PP4
    ring PP4
    PP4' = normalToricVariety(rays PP4, max PP4, CoefficientRing => ZZ/32003, Variable => y)
    ring PP4'
    ///,   
  PARA{},
  "The optional argument ", TO WeilToClass, " allows one to specify the map 
  from the group of torus-invariant Weil divisors to the class group.  In 
  particular, this allows the user to choose her favourite basis for the 
  class group.  This map also determines the grading on the total coordinate 
  ring of the toric variety.  For example, we can choose the opposite 
  generator for the class group of projective space as follows.",
  EXAMPLE lines ///
    PP2 = projectiveSpace 2;
    A = fromWDivToCl PP2
    source A == wDiv PP2
    target A == cl PP2
    degrees ring PP2
    deg = matrix {toList(3:-1)}
    X = normalToricVariety(rays PP2, max PP2, WeilToClass => deg);
    A' = fromWDivToCl X
    source A' == wDiv X
    target A' == cl X	  
    degrees ring X
    (matrix A')*(matrix rays X)
    ///,
  "The integer matrix ", TT "A", " should span the kernel of the matrix whose
  columns are the minimal nonzero lattice points on the rays of the fan.",
  PARA{},	  
  "We can also choose a basis for the class group of a blow-up of the 
  projective plane such that the nef cone is the positive quadrant.",
  EXAMPLE lines ///
    Rho = {{1,0},{0,1},{-1,1},{-1,0},{0,-1}};
    Sigma = {{0,1},{1,2},{2,3},{3,4},{0,4}};
    Y = normalToricVariety(Rho,Sigma);
    fromWDivToCl Y
    nef Y
    deg = matrix{{1,-1,1,0,0},{0,1,-1,1,0},{0,0,1,-1,1}}
    Y' = normalToricVariety(rays Y, max Y, WeilToClass => deg);	  
    fromWDivToCl Y'
    nef Y'
    ///,	  	  
  Caveat => {"This method assumes that the lists ", TT "Rho", " and ", 
    TT "Sigma", " correctly encode a strongly convex rational polyhedral fan.
    One can verify this by using ", TO (isWellDefined,NormalToricVariety), "."},
  SeeAlso => {
    "Making normal toric varieties",
    (rays, NormalToricVariety), 
    (max,NormalToricVariety),
    (isWellDefined,NormalToricVariety)}}

document { 
  Key => {(normalToricVariety,Matrix),
    [normalToricVariety,MinimalGenerators]},
  Headline => "make a normal toric variety from a polytope",
  Usage => "normalToricVariety V",
  Inputs => {
    "V" => Matrix => "of integers; each column is the lattice vertex of the 
                     polytope",
    CoefficientRing => Ring => "the coefficient ring of the total coordinate
                               ring",
    MinimalGenerators => Boolean => "an option which specifics whether to
                                    compute minimal generators",
    Variable => Symbol => "the base symbol for the indexed variables in the
                          total coordinate ring",
    WeilToClass => Matrix => "allows one to specify the map from the group of
                             torus-invariant Weil divisors to the class 
			     group",
    },
  Outputs => {NormalToricVariety},
  "This method makes a ", TO NormalToricVariety, " from the polytope with
  vertices corresponding to the columns of the matrix ", TT "V", ".  In
  particular, the associated fan is the inner normal fan of the polytope.",
  PARA{},
  "The first example shows how projective 2-space is obtained from a triangle",
  EXAMPLE lines ///
    PP2 = normalToricVariety matrix {{0,1,0},{0,0,1}};
    rays PP2
    max PP2
    PP2' = projectiveSpace 2;
    set rays PP2 === set rays PP2'
    max PP2 === max PP2'
    ///,
  "The second example makes the toric variety associated to the hypercube in
  affine 3-space",
  EXAMPLE lines ///
    X = normalToricVariety (id_(ZZ^3) | -id_(ZZ^3));
    isSimplicial X
    transpose matrix rays X, max X
    ///,
  "The optional argument ", TT "MinimalGenerators", " specifics whether to
  compute the vertices of the polytope defined as the convex hull of the 
  columns of the matrix ",  TT "A", ".",
  EXAMPLE lines ///
    FF1 = normalToricVariety matrix {{0,1,0,2},{0,0,1,1}};
    rays FF1
    max FF1 
    FF1' = hirzebruchSurface 1;
    rays FF1 === rays FF1'
    max FF1 === max FF1'
    V = matrix {{0,0,1,1,2},{0,1,0,1,1}}
    notFF1 = normalToricVariety V;
    max notFF1
    isWellDefined notFF1
    FF1'' = normalToricVariety(V, MinimalGenerators => true);
    rays FF1'' == rays FF1
    max FF1'' == max FF1
    ///,
  SeeAlso => {
    "Making normal toric varieties",
    (rays, NormalToricVariety), 
    (max,NormalToricVariety),
    (isWellDefined,NormalToricVariety),
    (vertices,ToricDivisor),
    (latticePoints,ToricDivisor)}}

document { 
  Key => {(isWellDefined,NormalToricVariety)},
  Headline => "whether a toric variety is well-defined",
  Usage => "isWellDefined X",
  Inputs => {"X" => NormalToricVariety},
  Outputs => {{TO2(true,"true"), " if the lists of rays and maximal cones
	       associated to ", TT "X", " determine a strongly convex rational
	       polyhedral fan" }},
  "A pair of lists ", TT "(Rho,Sigma)", " correspond to a well-defined normal
  toric variety if the following conditions hold:",
  UL {
    {"the union of the elements of ", TT "Sigma", " equals the set of indices 
      of elements of ", TT "Rho"},
    {"no element of ", TT "Sigma", " is properly contained in another element 
      of ", TT "Sigma"},     
    {"all elements of ", TT "Rho", " have the same length"},
    {"all elements of ", TT "Rho", " are lists of integers"},
    {"the rays indexed by an element of ", TT "Sigma", " generate a strongly
     convex cone"},
    {"the rays indexed by an element of ", TT "Sigma", " are the unique 
      minimal lattice points for the cone they generate"},
     {"the intersection of the cones associated to two elements of ", 
      TT "Sigma", " is a face of each cone."}},
  PARA{},
  "The first examples illustrate that small projective spaces are
  well-defined.",
  EXAMPLE lines ///
    for d from 1 to 6 list isWellDefined projectiveSpace d
    ///,     	  
   "The second examples show that a randomly selected Kleinschmidt toric 
   variety and a weighted projective space are also well-defined.",
   EXAMPLE lines ///
     setRandomSeed(currentTime());
     a = sort apply(3, i -> random(7))
     isWellDefined kleinschmidt(4,a)
     ///,
   EXAMPLE {
     "q = apply(5, j -> random(1,9));",
     "while not all(subsets(q,#q-1), s -> gcd s === 1) do (
       q = apply(5, j -> random(1,9)));",
     "q",
     "isWellDefined weightedProjectiveSpace q"
     },
   "The next eight examples illustrate various ways that two lists can fail to
   define a normal toric variety.  By making the current debugging level 
   greater than one, one gets some addition information about the nature of 
   the failure.",
   EXAMPLE lines ///
     Sigma = max projectiveSpace 2;
     X1 = normalToricVariety({{-1,-1},{1,0},{0,1},{-1,0}},Sigma);
     isWellDefined X1
     debugLevel = 1;
     isWellDefined X1	  	  
     ///,
   EXAMPLE lines ///
     Sigma' = {{0,1},{0,3},{1,2},{2,3},{3}};
     X2 = normalToricVariety({{-1,0},{0,-1},{1,-1},{0,1}},Sigma');
     isWellDefined X2
     ///,
   EXAMPLE lines ///	  
     X3 = normalToricVariety({{-1,-1},{1,0},{0,1,1}},Sigma);
     isWellDefined X3
     ///,
   EXAMPLE lines ///	  
     X4 = normalToricVariety({{-1,-1/1},{1,0},{0,1}},Sigma);
     isWellDefined X4
     ///,
   EXAMPLE lines ///	  
     X5 = normalToricVariety({{1,0},{0,1},{-1,0}},{{0,1,2}});
     isWellDefined X5
     ///,
   EXAMPLE lines ///	  
     X6 = normalToricVariety({{1,0},{0,1},{1,1}},{{0,1,2}});
     isWellDefined X6
     ///,
   EXAMPLE lines ///	  
     X7 = normalToricVariety({{1,0,0},{0,1,0},{0,0,2}},{{0,1,2}});
     isWellDefined X7
     ///,
   EXAMPLE lines ///	  
     X8 = normalToricVariety({{1,0},{0,1},{1,1}},{{0,1},{1,2}});
     isWellDefined X8
     ///,
   SeeAlso => {
     "Making normal toric varieties",
     (normalToricVariety,List,List)}}   	

document { 
  Key => {affineSpace, 
    (affineSpace,ZZ),
    [affineSpace,CoefficientRing],
    [affineSpace,Variable]
    },
  Headline => "make an affine space",
  Usage => "affineSpace d",
  Inputs => {"d" => "a positive integer",
    CoefficientRing => Ring => "the coefficient ring of the total coordinate 
                               ring",
    Variable => Symbol => "the base symbol for the indexed variables in the
                          total coordinate ring",},
  Outputs => {NormalToricVariety => {"affine ", TT "d", "-space"}},
  "Affine ", TEX ///$d$///, "-space is a smooth normal toric variety.  The 
  rays are generated by the standard basis ", TEX ///$e_1,\dots,e_d$///, 
  " of ", TEX ///$\ZZ^d$///, " and the maximal cone in the fan correspond to 
  the ", TEX ///$d$///, "-element subset of ", TEX ///$\{0,...,d-1\}$///, ".",
  PARA{},
  "The examples illustrate the affine line and projective ", TEX ///$3$///, 
  "-space.",
  EXAMPLE lines ///
    AA1 = affineSpace 1;
    rays AA1
    max AA1
    dim AA1
    isComplete AA1
    isSmooth AA1    
    ///,
  EXAMPLE lines ///
    AA3 = affineSpace(3, CoefficientRing => ZZ/32003, Variable => y);
    rays AA3
    max AA3
    dim AA3
    ring AA3
    isComplete AA3    
    isSmooth AA3    
    ///,	  
  SeeAlso => {
    "Making normal toric varieties",    
    (isSmooth,NormalToricVariety), 
    (isComplete,NormalToricVariety),
    (makeSmooth,NormalToricVariety)}}     

document { 
  Key => {projectiveSpace, 
    (projectiveSpace,ZZ),
    [projectiveSpace,CoefficientRing],
    [projectiveSpace,Variable]},
  Headline => "make a projective space",
  Usage => "projectiveSpace d",
  Inputs => {"d" => "a positive integer",
    CoefficientRing => Ring => "the coefficient ring of the total coordinate 
                               ring",
    Variable => Symbol => "the base symbol for the indexed variables in the
                          total coordinate ring",},
  Outputs => {NormalToricVariety => {"projective ", TT "d", "-space"}},
  "Projective ", TEX ///$d$///, "-space is a smooth complete normal toric
  variety.  The rays are generated by the standard basis ", 
  TEX ///$e_1,\dots,e_d$///, " of ", TEX ///$\ZZ^d$///, " together with ", 
  TEX ///$-e_1-\dots-e_d$///, ".  The maximal cones in the fan correspond to 
  the ", TEX ///$d$///, "-element subsets of ", TEX ///$\{0,...,d\}$///, ".",
  PARA{},
  "The examples illustrate the projective line and projective ", 
  TEX ///$3$///, "-space.",
  EXAMPLE lines ///
    PP1 = projectiveSpace 1;
    rays PP1
    max PP1
    dim PP1
    isComplete PP1
    isSmooth PP1
    ring PP1
    ideal PP1
    ///,
  EXAMPLE lines ///
    PP3 = projectiveSpace(3, CoefficientRing => ZZ/32003, Variable => y);
    rays PP3
    max PP3
    dim PP3
    isComplete PP3
    isSmooth PP3
    ring PP3
    ideal PP3
    ///,	  
  SeeAlso => {
    "Making normal toric varieties",    
    (isComplete,NormalToricVariety),
    (isSmooth,NormalToricVariety),
    (ring,NormalToricVariety), 
    (ideal,NormalToricVariety)}}     

document { 
  Key => {hirzebruchSurface, 
    (hirzebruchSurface,ZZ),
    [hirzebruchSurface,CoefficientRing],
    [hirzebruchSurface,Variable]},
  Headline => "make a Hirzebruch surface",
  Usage => "hirzebruchSurface a",
  Inputs => {"a" => ZZ,
    CoefficientRing => Ring => "the coefficient ring of the total coordinate 
                               ring",
    Variable => Symbol => "the base symbol for the indexed variables in the
                          total coordinate ring",},
  Outputs => {NormalToricVariety => "a Hirzebruch surface"},    
  "The ", TEX ///$a^{th}$///, " Hirzebruch surface is a complete normal toric
  variety.  It can be defined as the ", TEX ///$\PP^1$///, "-bundle over ", 
  TEX ///$X = \PP^1$///, " associated to the sheaf ", 
  TEX ///${\mathcal O}_X(0) \oplus {\mathcal O}_X(a)$///, ".  It is also the 
  quotient of affine ", TEX ///$4$///, "-space by a rank two torus.",
  EXAMPLE lines ///
    FF3 = hirzebruchSurface 3;
    rays FF3
    max FF3
    dim FF3
    ring FF3
    degrees ring FF3
    ideal FF3
    ///,
  "When ", TEX ///a = 0///, ", we obtain ", TEX ///$\PP^1 \times \PP^1$///, 
  ".",
  EXAMPLE lines ///
    FF0 = hirzebruchSurface(0, CoefficientRing => ZZ/32003, Variable => y);
    rays FF0
    max FF0
    dim FF0
    ring FF0
    degrees ring FF0
    I = ideal FF0
    decompose I
    ///,
  "The map from the torus-invariant Weil divisors to the class group is chosen
  so that the positive orthant corresponds to the cone of nef line bundles.",
  SeeAlso => {
    "Making normal toric varieties",    
    (ring,NormalToricVariety)}}     

document { 
  Key => {weightedProjectiveSpace, 
    (weightedProjectiveSpace,List),
    [weightedProjectiveSpace,CoefficientRing],
    [weightedProjectiveSpace,Variable]},
  Headline => "make a weighted projective space",
  Usage => "weightedProjectiveSpace q",
  Inputs => { "q" => {" a ", TO2(List,"list"), " of relatively prime positive
              integers"},
    CoefficientRing => Ring => "the coefficient ring of the total coordinate 
                               ring",
    Variable => Symbol => "the base symbol for the indexed variables in the
                          total coordinate ring",},
  Outputs => {NormalToricVariety => "a weighted projective space"},
  "The weighted projective space associated to a list ", TEX ///$\{q_0,\dots,
  q_d \}$///, ", where no ", TEX ///$d$///, "-element subset of ", 
  TEX ///$q_0,\dots, q_d$///, " has a nontrivial common factor, is a normal
  toric variety built from a fan in ", TEX ///$N =
  \ZZ^{d+1}/\ZZ(q_0,\dots,q_d)$///, ".  The rays are generated by the images 
  of the standard basis for ", TEX ///$\ZZ^{d+1}$///, " and the maximal cones 
  in the fan correspond to the ", TEX ///$d$///, "-element subsets of ", 
  TEX ///$\{0,...,d\}$///, ".",
  PARA{},
  "The first examples illustrate the defining data for three different 
  weighted projective spaces.",
  EXAMPLE lines ///
    PP4 = weightedProjectiveSpace {1,1,1,1};
    rays PP4
    max PP4
    dim PP4
    X = weightedProjectiveSpace {1,2,3};
    rays X
    max X
    dim X
    ring X
    Y = weightedProjectiveSpace({1,2,2,3,4}, CoefficientRing => ZZ/32003, Variable => y);
    rays Y
    max Y
    dim Y
    ring Y
    ///,
  "The grading of the total coordinate ring for weighted projective space is
  determined by the weights.  In particular, the class group is ", 
  TEX ///$\ZZ$///, ".",
  EXAMPLE lines ///
    cl PP4
    degrees ring PP4
    cl X
    degrees ring X
    cl Y
    degrees ring Y
    ///,
  "A weighted projective space is always simplicial but is typically not
  smooth",
  EXAMPLE lines ///
    isSimplicial PP4
    isSmooth PP4
    isSimplicial X
    isSmooth X
    isSimplicial Y
    isSmooth Y
    ///,
  SeeAlso => {
    "Making normal toric varieties",    
    (projectiveSpace,ZZ), 
    (ring,NormalToricVariety), 
    (cl,NormalToricVariety),
    (isSimplicial,NormalToricVariety), 
    (isSmooth,NormalToricVariety)}}     

document { 
  Key => {(symbol **,NormalToricVariety,NormalToricVariety)},
  Headline => "Cartesian product",
  Usage => "X ** Y",
  Inputs => {"X", "Y" => NormalToricVariety },
  Outputs => {{"the product of ", TT "X", " and ", TT "Y"}},     
  "The Cartesian product of two varieties ", TEX ///$X$///, " and ", 
  TEX ///$Y$///, ", both defined over the same ground field ", TEX ///$k$///, 
  ", is the fiber product ", TEX ///$X \times_k Y$///, ".  For normal toric
  varieties, the fan of the product is given by the Cartesian product of each
  pair of cones in the fans of the factors.",
  EXAMPLE lines ///
    PP2 = projectiveSpace 2;
    FF2 = hirzebruchSurface 2;
    X = FF2 ** PP2;
    #rays X == #rays FF2 + #rays PP2
    transpose matrix rays X
    transpose matrix rays FF2 ++ transpose matrix rays PP2
    primaryDecomposition ideal X
    flatten (primaryDecomposition \ {ideal FF2,ideal PP2})
    ///,
  "The map from the torus-invariant Weil divisors to the class group is the 
  direct sum of the maps for the factors",
  EXAMPLE lines ///
    fromWDivToCl FF2 ++ fromWDivToCl PP2
    fromWDivToCl X
  ///,
  SeeAlso => {
    "Making normal toric varieties", 
    (symbol ^**, NormalToricVariety, ZZ),   
    normalToricVariety}}  

document { 
  Key => {(symbol ^**,NormalToricVariety,ZZ)},
  Headline => "Cartesian power",
  Usage => "X ^** i",
  Inputs => {
    "X" => NormalToricVariety,
    "i" => ZZ, },
  Outputs => {{"the ", TT "i", "-ary Cartesian product of ", TT "X", " with 
      itself "}},     
  "The ", TT "i", "-ary Cartesian product of the variety ", TEX ///$X$///, 
  ", defined over the ground field ", TEX ///$k$///, ", is the ", 
  TEX ///$i$///, "-ary fiber product of ", TEX ///$X$///, " with itself 
  over ", TEX ///$k$///, ".  For a normal toric variety, the fan of the ",
  TEX ///$i$///, "-ary Cartesian product is given by the ",  TEX ///$i$///, 
  "-ary Cartesian product of the cones.",
  EXAMPLE lines ///
    PP2 = projectiveSpace 2;
    X = PP2 ^** 4;
    degrees ring X
    fromWDivToCl X
    FF2 = hirzebruchSurface(2) ;
    Y = FF2 ^** 3;
    degrees ring Y
    fromWDivToCl Y
    X' = PP2 ** PP2;
    X'' = PP2 ^** 2;
    rays X' == rays X''
    max X' == max X''  
    ///,
  SeeAlso => {
    "Making normal toric varieties", 
    (symbol **, NormalToricVariety, NormalToricVariety),          
    normalToricVariety}}  

document { 
  Key => {kleinschmidt, 
    (kleinschmidt,ZZ,List),
    [kleinschmidt,CoefficientRing],
    [kleinschmidt,Variable]},
  Headline => "make a smooth toric variety with Picard rank two",
  Usage => "kleinschmidt(d,a)",
  Inputs => {
    "d" => ZZ => " dimension of toric variety",
    "a" => {" an increasing list of at most ", TT "d-1", "nonnegative 
      integers"},
    CoefficientRing => Ring => "the coefficient ring of the total coordinate 
                               ring",
    Variable => Symbol => "the base symbol for the indexed variables in the
                          total coordinate ring",},
  Outputs => {NormalToricVariety => "a smooth toric variety with Picard rank
                                    two"},
  "Peter Kleinschmidt constructs (up to isomorphism) all smooth normal toric
  varieties with dimension ", TEX ///$d$///, " and ", TEX ///$d+2$///, " rays;
  see P. Kleinschmidt, A classification of toric varieties with few 
  generators, ", EM "Aequationes Mathematicae ", STRONG "35", " (1998) 
  254-266.", 
  PARA{},
  "When ", TEX ///$d=2$///, ", we obtain a variety isomorphic to a Hirzebruch
  surface.",
  EXAMPLE lines ///
    X = kleinschmidt(2,{3});
    rays X
    max X
    FF3 = hirzebruchSurface 3;
    rays FF3
    max FF3
    ///,
  "The normal toric variety associated to the pair ", TEX ///$(d,A)$///, " is
  Fano if and only if ", TEX ///\sum_{i=0}^{r-1} a_i < d-r+1///, ".",
  EXAMPLE lines ///
    X1 = kleinschmidt(3,{0,1});	  
    isFano X1
    X2 = kleinschmidt(4,{0,0});	  
    isFano X2
    ring X2
    X3 = kleinschmidt(9,{1,2,3}, CoefficientRing => ZZ/32003, Variable => y);
    isFano X3
    ring X3
    ///,
  SeeAlso => {
    "Making normal toric varieties",
    normalToricVariety}}    

document { 
  Key => {smoothFanoToricVariety, 
    (smoothFanoToricVariety,ZZ,ZZ),
    [smoothFanoToricVariety,CoefficientRing],
    [smoothFanoToricVariety,Variable]
    },
  Headline => "get a smooth Fano toric variety from database",
  Usage => "smoothFanoToricVariety(d,i)",
  Inputs => {
    "d" => ZZ => " dimension of toric variety",
    "i" => ZZ => " index of toric variety in database",
    CoefficientRing => Ring => "the coefficient ring of the total coordinate 
                               ring",
    Variable => Symbol => "the base symbol for the indexed variables in the
                          total coordinate ring",},
  Outputs => {NormalToricVariety => " a smooth Fano toric variety"},
  "This function accesses a database of all smooth Fano toric varieties of
  dimension at most 6.  The enumeration of the toric varieties follows ",
  HREF("http://www.mathematik.uni-tuebingen.de/~batyrev/batyrev.html.en",
       "Victor V. Batyrev's"),	
  " classification (see ", 
  HREF("http://arxiv.org/abs/math/9801107", TT "arXiv:math/9801107v2"), 
  " and ",
  HREF("http://arxiv.org/abs/math/9911022", TT "arXiv:math/9011022"),   
  ") for dimension at most 4 and Mikkel Øbro's classification (see ",
  HREF("http://arxiv.org/abs/0704.0049", TT "arXiv:math/0704.0049v1"), ")
  for dimensions 5 and 6.  There is a unique smooth Fano toric curve, five
  smooth Fano toric surfaces, eighteen smooth Fano toric threefolds, ", 
  TEX ///$124$///, " smooth Fano toric fourfolds, ", TEX ///$866$///, " smooth
  Fano toric fivefolds, and ", TEX ///$7622$///, " smooth Fano toric 
  sixfolds.",
  PARA{},
  "For all ", TEX ///$d$///, ", ", TT "smoothFanoToricVariety(d,0)", " yields
  projective ", TEX ///$d$///, "-space.",
  EXAMPLE lines ///
    PP1 = smoothFanoToricVariety(1,0);
    rays PP1	  
    max PP1
    ring PP1
    PP4 = smoothFanoToricVariety(4,0, CoefficientRing => ZZ/32003, Variable => y);
    rays PP4
    max PP4
    ring PP4
    ///,
  "The following example was missing from Batyrev's table.",
  EXAMPLE lines ///
    W = smoothFanoToricVariety(4,123);
    rays W
    max W
    ///,
  SUBSECTION "Acknowledgements",
  "We thank ", HREF("http://www-staff.lboro.ac.uk/~magdb/", "Gavin Brown"), "
  and ",
  HREF("http://magma.maths.usyd.edu.au/users/kasprzyk/index.html","Alexander
  Kasprzyk"), " for their help extracting the data for the smooth Fano toric
  five and sixfolds from their ", HREF("http://grdb.lboro.ac.uk/", "Graded 
  Rings Database"), ".",
  SeeAlso => {
    "Making normal toric varieties",
    normalToricVariety,
    (isFano,NormalToricVariety)}}     

document {
  Key => (normalToricVariety,Fan),
  Headline => "make a normal toric variety from a 'Polyhedra' fan",
  Usage => " X = normalToricVariety F",
  Inputs => {
    "F" => Fan,
    CoefficientRing => Ring => "the coefficient ring of the total coordinate
                               ring",
    MinimalGenerators => Boolean => "an option which specifics whether to
                                    compute minimal generators",
    Variable => Symbol => "the base symbol for the indexed variables in the
                          total coordinate ring",
    WeilToClass => Matrix => "allows one to specify the map from the group of
                             torus-invariant Weil divisors to the class 
			     group",
    },
  Outputs => {"X" => NormalToricVariety},
  "This method makes a ", TO NormalToricVariety, " from a ", 
  TO "Polyhedra::Fan", " as implemented in the ", TO "Polyhedra::Polyhedra",
  " package. ",
  EXAMPLE lines ///
    F = faceFan hypercube 3
    X = normalToricVariety F;
    rays X
    max X
    ///,  
  SUBSECTION "Remark",
  "The recommended method for creating a ", TO NormalToricVariety, " from a 
  fan is ", TO (normalToricVariety,List,List), ".  In fact, this package 
  avoids using objects from the ", TO "Polyhedra::Polyhedra", " whenever 
  possible because their significant overhead.  For example, creating a ", 
  TO "Polyhedra::Fan", " requires computing the polar dual (twice) for each 
  cone in the fan.  Here is a trivial example, namely projective 2-space,
  illustrating the substantial increase in time resulting from the use of a ",
  TO "Polyhedra::Polyhedra", " fan.",
  EXAMPLE {
    "X1 = time normalToricVariety({{-1,-1},{1,0},{0,1}}, {{0,1},{1,2},{0,2}});",
    "rays X1",    
    "max X1",
    "X2 = time normalToricVariety fan {
      posHull matrix {{-1,1},{-1,0}}, 
      posHull matrix {{1,0},{0,1}}, 
      posHull matrix{{-1,0},{-1,1}}};",
    "rays X2",
    "max X2"},
  SeeAlso => {
    "Making normal toric varieties",
    normalToricVariety}}

document {
  Key => (normalToricVariety,Polyhedron),
  Headline => "make a normal toric variety from a 'Polyhedra' polyhedron",
  Usage => " X = normalToricVariety P",
  Inputs => {
    "P" => Polyhedron,
    CoefficientRing => Ring => "the coefficient ring of the total coordinate
                               ring",
    MinimalGenerators => Boolean => "an option which specifics whether to
                                    compute minimal generators",
    Variable => Symbol => "the base symbol for the indexed variables in the
                          total coordinate ring",
    WeilToClass => Matrix => "allows one to specify the map from the group of
                             torus-invariant Weil divisors to the class 
			     group",
    },
  Outputs => {"X" => NormalToricVariety},
  "This method makes a ", TO NormalToricVariety, " from a ", 
  TO "Polyhedra::Polyhedron", " as implemented in the ", 
  TO "Polyhedra::Polyhedra", " package.  In particular, the associated fan is
  outer normal fan to the polyhedron.",
  EXAMPLE lines ///
    P = hypercube 3
    X = normalToricVariety P;
    rays X
    max X
    ///,    
  SUBSECTION "Remark",
  "The recommended method for creating a ", TO NormalToricVariety, " from a
  polytope is ", TO (normalToricVariety,Matrix), ".  In fact, this package
  avoids using objects from the ", TO "Polyhedra::Polyhedra", " whenever
  possible because their significant overhead.  Here is a trivial example,
  namely projective 2-space, illustrating the substantial increase in time
  resulting from the use of a ", TO "Polyhedra::Polyhedra", " polyhedron.",
  EXAMPLE lines ///
    V = matrix {{0,-1,0},{0,0,-1}}
    X1 = time normalToricVariety convexHull ((-1)*V);
    X2 = time normalToricVariety V;
    set rays X2 === set rays X1
    max X1 == max X2
    ///,
  SeeAlso => {
    "Making normal toric varieties",
    (normalToricVariety,Matrix)}}



------------------------------------------------------------------------------
-- basic properties and invariants

document { 
  Key => "Basic invariants and properties of normal toric varieties",
  "Having made a ", TO NormalToricVariety, ", one can access its basic
  invariants or test for some elementary properties by using the following
  methods:",
  SUBSECTION "Menu",
  UL {
    TO (rays, NormalToricVariety),
    TO (max, NormalToricVariety),    
    TO (expression, NormalToricVariety),    
    TO (dim,NormalToricVariety),
    TO (orbits,NormalToricVariety,ZZ),
    TO (isDegenerate,NormalToricVariety),
    TO (isSimplicial, NormalToricVariety),
    TO (isSmooth, NormalToricVariety),
    TO (isComplete, NormalToricVariety),
    TO (isProjective,NormalToricVariety),
    TO (isFano,NormalToricVariety),
    TO (fan,NormalToricVariety)},
  SeeAlso =>{
    "Making normal toric varieties",
    "Working with divisors and their associated groups",
    "Total coordinate rings and coherent sheaves",
    "Resolution of singularities"}}

document { 
  Key => {(rays, NormalToricVariety)},
  Headline => "get the rays of the associated fan",
  Usage => "rays X",
  Inputs => {"X" => NormalToricVariety},
  Outputs => {List => " of lists of integers; each entry corresponds to a
                      minimal nonzero lattice point on the ray in the fan" },
  "A normal toric variety corresponds to a strongly convex rational polyhedral
  fan in affine space.  ", "In this package, the fan associated to a normal ",
  TEX ///$d$///, "-dimensional toric variety lies in the rational vector space
  ", TEX ///$\QQ^d$///, " with underlying lattice ", TEX ///$N = {\ZZ}^d$///, 
  ".  As a result, each ray in the fan is determined by the minimal nonzero 
  lattice point it contains.  Each such lattice point is given as a ", 
  TO2(List,"list"), " of ", TEX ///$d$///, " ", TO2(ZZ,"integers"), ".",
  PARA{},
  "The examples show the rays for the projective plane, projective ", 
  TEX ///$3$///, "-space, a Hirzebruch surface, and a weighted projective
  space.  Observe that there is a bijection between the rays and 
  torus-invariant Weil divisor on the toric variety.",
  EXAMPLE lines ///
    PP2 = projectiveSpace 2;
    rays PP2
    dim PP2
    wDiv PP2
    ///,
  EXAMPLE lines ///
    PP3 = projectiveSpace 3;
    rays PP3
    dim PP3
    wDiv PP3
    ///,
  EXAMPLE lines ///
    FF7 = hirzebruchSurface 7;
    rays FF7
    dim FF7
    wDiv FF7
    ///,
  EXAMPLE lines ///
    X = weightedProjectiveSpace {1,2,3};
    rays X
    dim X
    wDiv X
    ///,     	    
  "When ", TT "X", " is nondegerenate, the number of rays equals the number of
  variables in the total coordinate ring.",
  EXAMPLE lines ///
    #rays X == numgens ring X
    ///,	  
  "An ordered list of the minimal nonzero lattice points on the
  rays in the fan is part of the defining data of a toric variety.",
  SeeAlso => {
    "Making normal toric varieties",
    "Basic invariants and properties of normal toric varieties", 
    (max, NormalToricVariety),
    (ring, NormalToricVariety)}}

document { 
  Key => {(max, NormalToricVariety)},
  Headline => "get the maximal cones in the associated fan",
  Usage => "max X",
  Inputs => {"X" => NormalToricVariety},
  Outputs => {List => " of lists of nonnegative integers; each entry indexes 
                       the rays which generate a maximal cone in the fan"},  
  "A normal toric variety corresponds to a strongly convex rational polyhedral
  fan in affine space.  ", "In this package, the fan associated to a normal ",
  TEX ///$d$///, "-dimensional toric variety lies in the rational vector space
  ", TEX ///$\QQ^d$///, " with underlying lattice ", TEX ///$N = \ZZ^d$///, ".
  The fan is encoded by the minimal nonzero lattice points on its rays and the
  set of rays defining the maximal cones (i.e. a maximal cone is not properly
  contained in another cone in the fan).  ", "The rays are ordered and indexed
  by nonnegative integers: ", TEX ///$0,\dots, n$///, ".  Using this indexing,
   a maximal cone in the fan corresponds to a sublist of ", 
  TEX ///$\{0,\dots,n\}$///, "; the entries index the rays that generate the 
  cone.",
  PARA{},    
  "The examples show the maximal cones for the projective plane, projective
  3-space, a Hirzebruch surface, and a weighted projective space.",
  EXAMPLE lines ///
    PP2 = projectiveSpace 2;
    #rays PP2
    max PP2
    ///,
  EXAMPLE lines ///
    PP3 = projectiveSpace 3;
    #rays PP3
    max PP3
    ///,
  EXAMPLE lines ///
    FF7 = hirzebruchSurface 7;
    #rays FF7
    max FF7
    ///,
  EXAMPLE lines ///
    X = weightedProjectiveSpace {1,2,3};
    #rays X
    max X
  ///,   
  "A list corresponding to the maximal cones in the fan is part of the 
  defining data of a toric variety.",
  SeeAlso => {
    "Making normal toric varieties",
    "Basic invariants and properties of normal toric varieties",
    (rays, NormalToricVariety)}}     

document { 
  Key => {(expression, NormalToricVariety)},
  Headline => "get the expression used to format for printing",
  Usage => "expression X",
  Inputs => {"X" => NormalToricVariety },
  Outputs => {Expression => {" used to format ", TT "X", " for printing"}},
  "This function is the primary function called upon by ", TO(symbol <<), " to
  format for printing.  It displays the minimal nonzero lattice points on each
  ray and the subsets of rays which determine the maximal cones in the fan.",
  EXAMPLE lines ///
    projectiveSpace 3
    rays projectiveSpace 3
    max projectiveSpace 3
    ///,	
  EXAMPLE lines ///
    hirzebruchSurface 7
    rays hirzebruchSurface 7
    max hirzebruchSurface 7
    ///,		    
  "After assignment to a global variable, ", EM "Macaulay2", " knows the toric
  variety's name, and this name is used when printing.",
  EXAMPLE lines ///
    PP2 = projectiveSpace 3
    FF7 = hirzebruchSurface 7
    ///,
  SeeAlso => {
    "Basic invariants and properties of normal toric varieties",
    (rays,NormalToricVariety),
    (max,NormalToricVariety)}} 

document { 
  Key => {(dim, NormalToricVariety)},
  Headline => "get the dimension of a normal toric variety",
  Usage => "dim X",
  Inputs => {"X" => NormalToricVariety },
  Outputs => {ZZ => "the dimension of the normal toric variety"},  
  "The dimension of a normal toric variety equals the dimension of its dense
  algebraic torus.  In this package, the fan associated to a normal ", 
  TEX ///$d$///, "-dimensional toric variety lies in the rational vector space
  ", TEX ///$\QQ^d$///, " with underlying lattice ", TEX ///$N = \ZZ^d$///, ".
  Hence, the dimension equals the number of entries in a minimal nonzero
  lattice point on a ray.",
  PARA{},   
  "The following examples illustrate normal toric varieties of various
  dimensions.",
  EXAMPLE lines ///
    dim projectiveSpace 1
    dim projectiveSpace 5
    dim hirzebruchSurface 7
    dim weightedProjectiveSpace {1,2,2,3,4}
    W = normalToricVariety({{4,-1,0},{0,1,0}},{{0,1}})
    dim W
    isDegenerate W
    ///,
  SeeAlso => {
    "Basic invariants and properties of normal toric varieties",
    (rays, NormalToricVariety)}}  
 
document { 
  Key => {orbits,
    (orbits, NormalToricVariety)},
  Headline => "make a hashtable indexing the proper torus orbits",
  Usage => "orbits X",
  Inputs => {"X" => NormalToricVariety },
  Outputs => {HashTable => "whose keys are the dimensions of the proper torus
                           orbits and whose values are lists of lists of
                           integers indexing the proper torus orbits"},
  "A normal toric variety is a disjoint union of its orbits under the action 
  of its algebraic torus.  These orbits are in bijection with the cones in the
  associated fan.  Each cone is determined by the rays it contains.  In this
  package, the rays are ordered and indexed by nonnegative integers: ", 
  TEX ///$0,\dots,n$///, ". Using this indexing, an orbit or cone corresponds 
  to a sublist of ", TEX ///$\{0,\dots,n\}$///, "; the entries index the rays 
  that generate the cone.",
  PARA{},
  "Projective 2-space has three fixed points and three fixed curves (under the
  action of its torus), and projective 3-space has four fixed points, six 
  fixed curves, and four divisors.  More generally, the orbits of 
  projective ", TT "(n-1)", "-space are enumerated by the ", TT "n", "-th row 
  of Pascal's triangle.",
  EXAMPLE lines ///
    O2 = orbits projectiveSpace 2
    #O2#0
    #O2#1
    O3 = orbits projectiveSpace 3     
    apply(keys O3, k -> #O3#k)
    apply(4, k -> #(orbits projectiveSpace 4)#k)
    apply(5, k -> #(orbits projectiveSpace 5)#k)    
    ///,
  "Here is a non-simplicial example.",
  EXAMPLE lines ///
    X = normalToricVariety(id_(ZZ^3) | -id_(ZZ^3));
    isSimplicial X
    orbits X
    ///,
  "The following degenerate example has no fixed points",
  EXAMPLE lines ///
    U = normalToricVariety({{4,-1,0},{0,1,0}},{{0,1}});
    isDegenerate U
    orbits U
    ///,    
  SeeAlso => {
    "Basic invariants and properties of normal toric varieties",
    (rays, NormalToricVariety),
    (orbits,NormalToricVariety,ZZ)}}  

document { 
  Key => {(orbits, NormalToricVariety,ZZ)},
  Headline => "get a list of the torus orbits of a given dimension",
  Usage => "orbits(X,i)",
  Inputs => {
    "X" => NormalToricVariety,
    "i" => ZZ => "giving the dimension of the orbits"},
  Outputs => {List => "of lists of integers indexing the proper torus 
                       orbits"},
  "A normal toric variety is a disjoint union of its orbits under the action 
  of its algebraic torus.  These orbits are in bijection with the cones in the
  associated fan.  Each cone is determined by the rays it contains.  In this
  package, the rays are ordered and indexed by nonnegative integers: ", 
  TEX ///$0,\dots,n$///, ". Using this indexing, an orbit or cone corresponds 
  to a sublist of ", TEX ///$\{0,\dots,n\}$///, "; the entries index the rays 
  that generate the cone.",
  PARA{},
  "Projective 2-space has three fixed points and three fixed curves (under the
  action of its torus), and projective 3-space has four fixed points, six 
  fixed curves, and four divisors.",
  EXAMPLE lines ///
    PP2 = projectiveSpace 2;
    orbits(PP2,0)
    orbits(PP2,1)
    PP3 = projectiveSpace 3;
    orbits(PP3,0)
    orbits(PP3,1)
    orbits(PP3,2)
    ///,
  "Here is a non-simplicial example.  Since it is nondegenerate, the fixed
  points correspond to the maximal cones in the fan.  The rays always 
  correspond to the divisors.",
  EXAMPLE lines ///
    X = normalToricVariety(id_(ZZ^3) | -id_(ZZ^3));
    orbits(X,0) === max X
    orbits(X,1)
    orbits(X,2) === apply(#rays X, i -> {i})
    ///,
  "The following degenerate example has no fixed points",
  EXAMPLE lines ///
    U = normalToricVariety({{4,-1,0},{0,1,0}},{{0,1}});
    isDegenerate U
    orbits(U,0)
    orbits(U,1)
    orbits(U,2)
    dim U
    ///,
  SeeAlso => {
    "Basic invariants and properties of normal toric varieties",
    (rays, NormalToricVariety),
    (orbits,NormalToricVariety)}}  

document { 
  Key => {isDegenerate, 
    (isDegenerate,NormalToricVariety)},
  Headline => "whether a toric variety is degenerate",
  Usage => "isDegenerate X",
  Inputs => {"X" => NormalToricVariety},
  Outputs => {Boolean => {TO "true", " if the fan of ", TT "X", " is contained
	      in a proper subspace of its ambient space, and ", TO "false", "
	      otherwise" }},
  "A ", TEX ///$d$///, "-dimensional normal toric variety is degenerate if its
  rays do not span ", TEX ///$\QQ^d$///, ".  For example, projective spaces 
  and Hirzebruch surfaces are not degenerate.",
  EXAMPLE lines ///
    isDegenerate projectiveSpace 3
    isDegenerate hirzebruchSurface 7
    ///,
  "Although one typically works with non-degenerate toric varieties, not all
  normal toric varieties are non-degenerate.",
  EXAMPLE lines ///
    U = normalToricVariety({{4,-1,0},{0,1,0}},{{0,1}});
    isDegenerate U
    ///,
  SeeAlso => {
    "Basic invariants and properties of normal toric varieties",
    (rays,NormalToricVariety)}}

document { 
  Key => {(isSimplicial,NormalToricVariety)},
  Headline => "whether a toric variety is simplicial",
  Usage => "isSimplicial X",
  Inputs => {"X" => NormalToricVariety},
  Outputs => {Boolean => {TO "true", " if the minimal nonzero lattice points 
              on the rays in each maximal cone in the associated fan of form 
	      part of a ", TEX ///$\QQ$///, "-basis, and ", TO "false", 
	      " otherwise"
	      }},
  "A normal toric variety is simplical if every cone in its fan is simplicial
  and a cone is simplicial if its minimal generators are linearly independent
  over ", TEX ///$\QQ$///, ".  In fact, the following conditions on a normal
  toric variety ", TEX ///$X$///, " are equivalent:",
  UL{
    {TEX ///$X$///, " is simplicial;"},
    {"every Weil divisor on ", TEX ///$X$///, " has a positive integer 
      multiple that is Cartier;"},
    {TEX ///$X$///, " is ", TEX ///$\QQ$///, "-Cartier;"},
    {"the Picard group of ", TEX ///$X$///, " has finite index in the class
     group of ", TEX ///$X$///, ";"},
    {TEX ///$X$///, " has only finite quotient singularities."}},
  "Projective spaces, weighted projective spaces and Hirzebruch surfaces are
  simplicial.",
  EXAMPLE lines ///
    isSimplicial projectiveSpace 4
    isSimplicial weightedProjectiveSpace {1,2,3}
    isSimplicial hirzebruchSurface 7
    ///,
  "However, not all normal toric varieties are simplicial.",
  EXAMPLE lines ///
    U = normalToricVariety({{4,-1},{0,1}},{{0,1}});
    isSimplicial U
    isSmooth U
    ///,
  EXAMPLE lines ///
    C = normalToricVariety({{1,0,0},{0,1,0},{0,0,1},{1,1,-1}},{{0,1,2,3}});
    isSimplicial C
    ///,
  SeeAlso => {
    "Basic invariants and properties of normal toric varieties",
    (rays,NormalToricVariety), 
    (max, NormalToricVariety), 
    (isSmooth,NormalToricVariety),
    (makeSimplicial,NormalToricVariety)}}     

document { 
  Key => {(isSmooth,NormalToricVariety)},
  Headline => "whether a toric variety is smooth",
  Usage => "isSmooth X",
  Inputs => {"X" => NormalToricVariety},
  Outputs => {Boolean => {TO "true", " if the minimal nonzero lattice points 
              on the rays in each maximal cone in the associated fan of form 
	      part of a ", TEX ///$\ZZ$///, "-basis, and ", TO "false", 
	      " otherwise"}},
  "A normal toric variety is smooth if every cone in its fan is smooth and a
  cone is smooth if its minimal generators are linearly independent over ", 
  TEX ///$\ZZ$///, ".  In fact, the following conditions on a normal toric
  variety ", TEX ///$X$///, " are equivalent:",
  UL{
    {TEX ///$X$///, " is smooth;"},
    {"every Weil divisor on ", TEX ///$X$///, " is Cartier;"},
    {"the Picard group of ", TEX ///$X$///, " equals the class group of ", 
     TEX ///$X$///, ";"},
    {TEX ///$X$///, " has no singularities."}},
  "Projective spaces and Hirzebruch surfaces are smooth.",
  EXAMPLE lines ///
    isSmooth projectiveSpace 4
    isSmooth hirzebruchSurface 7
    ///,
  "However, not all normal toric varieties are smooth.",  
  EXAMPLE lines ///
    isSmooth weightedProjectiveSpace {1,2,3}
    ///,  
  EXAMPLE lines ///
    U = normalToricVariety({{4,-1},{0,1}},{{0,1}});
    isSimplicial U
    isSmooth U
    ///,
  EXAMPLE lines ///
    U' = normalToricVariety({{4,-1},{0,1}},{{0},{1}});
    isSmooth U'
    ///,
  SeeAlso => {
    "Basic invariants and properties of normal toric varieties",    
    (rays,NormalToricVariety), 
    (max, NormalToricVariety),
    (isSimplicial,NormalToricVariety)}}     

document {
  Key => {isFano, (isFano,NormalToricVariety)},
  Headline => "whether a normal toric variety is Fano",
  Usage => "isFano X",
  Inputs => {"X" => NormalToricVariety},
  Outputs => {Boolean => {"true", " if the normal toric variety is Fano, 
              and ", TO "false", " otherwise"}},
  "A normal toric variety is Fano if its anticanonical divisor, namely the sum
  of all the torus-invariant prime divisors, is ample.  This is equivalent to
  saying that the polyhedron associated to the anticanonical divisor is
  a reflexive polytope.",
  PARA{},
  "Projective space is Fano.", 
  EXAMPLE lines ///
    PP3 = projectiveSpace 3;
    isFano PP3
    K = toricDivisor PP3
    isAmple (-K)
    apply(5, d -> isFano projectiveSpace (d+1))
    ///,
  "There are eighteen smooth Fano toric threefolds.",
  EXAMPLE lines ///
    all(18, i -> (X := smoothFanoToricVariety(3,i); isSmooth X and isFano X))
    ///,
  "There are also many singular Fano toric varieties",
  EXAMPLE lines ///
    X = normalToricVariety matrix {{1,0,-1},{0,1,-1}};
    isSmooth X
    isFano X
    Y = normalToricVariety matrix {{1,1,-1,-1},{0,1,1,-1}}
    isSmooth Y
    isFano Y
    Z = normalToricVariety(id_(ZZ^3) | -id_(ZZ^3));
    isSmooth Z
    isFano Z
    ///,
  SeeAlso => {
    "Basic invariants and properties of normal toric varieties",    
    (toricDivisor,NormalToricVariety),
    (isAmple,ToricDivisor),
    (smoothFanoToricVariety,ZZ,ZZ)}}

document {
  Key => {(isComplete,NormalToricVariety)},
  Headline => "whether a toric variety is complete",
  Usage => "isComplete X",
  Inputs => {"X" => NormalToricVariety},
  Outputs => {Boolean => {TO true, " if the normal toric variety is complete,
                          and ", TO false, " otherwise"}},
  "A normal toric variety is complete if any of the following equivalent
  conditions hold:",
  UL {
    {"the associated complex variety is compact in its classical topology;"},
    {"the constant map from the normal toric variety to space consisting of a
      single point is proper;"}, 
    {"every one-parameter subgroup of the torus has a limit in the toric
      variety;"},
    {"the union of all the cones in the associated fan equals the entire 
      vector space containing it;"},
    {"every torus-invariant curve lying in the normal toric variety is
      projective."}},
  PARA{},
  "Affine varieties are not complete.",
  EXAMPLE lines ///
    isComplete affineSpace 1
    isComplete affineSpace 3
    U = normalToricVariety({{4,-1,0},{0,1,0}},{{0,1}});
    isComplete U
    ///,
  "Projective varieties are complete.",
  EXAMPLE lines ///
    isComplete projectiveSpace 1
    isComplete projectiveSpace 3
    isComplete hirzebruchSurface 7
    ///,
  "There are also complete non-projective normal toric varieties.",
  EXAMPLE lines ///
    R1 = {{1,0,0},{0,1,0},{0,0,1},{0,-1,-1},{-1,0,-1},{-2,-1,0}};
    S1 = {{0,1,2},{0,1,3},{1,3,4},{1,2,4},{2,4,5},{0,2,5},{0,3,5},{3,4,5}};
    X1 = normalToricVariety(R1,S1);
    isComplete X1
    isProjective X1
    isSmooth X1
    ///,
  EXAMPLE lines ///
    R2 = {{1,0,0},{0,1,0},{0,0,1},{0,-1,2},{0,0,-1},{-1,1,-1},{-1,0,-1},{-1,-1,0}};
    S2 = {{0,1,2},{0,2,3},{0,3,4},{0,4,5},{0,1,5},{1,2,7},{2,3,7},{3,4,7},{4,5,6},{4,6,7},{5,6,7},{1,5,7}};  
    X2 = normalToricVariety(R2,S2);    
    isComplete X2
    isProjective X2 
    isSmooth X2
    ///,
  EXAMPLE lines ///    
    R3 = {{-1,2,0},{0,-1,0},{1,-1,0},{-1,0,-1},{0,0,-1},{0,1,0},{0,0,1},{1,0,-2}};
    S3 = {{0,1,3},{1,2,3},{2,3,4},{3,4,5},{0,3,5},{0,5,6},{0,1,6},{1,2,6},{2,4,7},{4,5,7},{2,6,7},{5,6,7}};
    X3 = normalToricVariety(R3,S3);    
    isComplete X3
    isProjective X3 
    isSmooth X3    
    ///,
  "The nonprojective examples are taken from Osamu Fujino and Sam Payne's paper
  ", HREF("http://projecteuclid.org/euclid.pja/1135791770", "Smooth complete
  toric threefolds with non nontrivial nef line bundles"), EM " Proc. Japan
  Acad. Ser. A Math. Sci.", BOLD " 81 ", " (2005), no. 10, 174--179.",
  SeeAlso => {
    "Basic invariants and properties of normal toric varieties",
    (isProjective,NormalToricVariety)}}  
  
document { 
  Key => {isProjective, 
    (isProjective,NormalToricVariety)},
  Headline => "whether a toric variety is projective",
  Usage => "isProjective X",
  Inputs => {"X" => NormalToricVariety},
  Outputs => {{TO2(true,"true"), " if ", TT "X", " is a projective variety,
               and ", TO2(false, "false"), " otherwise"}},
  "A variety is projective if it can be realized as a closed subvariety of 
  some projective space.  For an normal toric variety, this is equivalent to 
  saying that the associated fan is the normal fan of a polytope.",  
  PARA{},     
  "Nontrivial affine varieties are not projective.",
  EXAMPLE lines ///
    isProjective affineSpace 1
    isProjective affineSpace 3
    U = normalToricVariety({{4,-1,0},{0,1,0}},{{0,1}});
    isProjective U
    ///,
  "Many of our favour toric varieties are projective.",
  EXAMPLE lines ///
    isProjective projectiveSpace 1
    isProjective projectiveSpace 3
    isProjective hirzebruchSurface 7
    isProjective smoothFanoToricVariety(3,3)
    X = normalToricVariety(id_(ZZ^3) | -id_(ZZ^3));
    isProjective X
    ///,
  "There are complete non-projective normal toric varieties.",
  EXAMPLE lines ///
    Rho = {{-1,-1,1},{3,-1,1},{0,0,1},{1,0,1},{0,1,1},{-1,3,1},{0,0,-1}};
    Sigma = {{0,1,3},{0,1,6},{0,2,3},{0,2,5},{0,5,6},{1,3,4},{1,4,5},{1,5,6},{2,3,4},{2,4,5}}
    Y = normalToricVariety(Rho,Sigma);
    isComplete Y
    isProjective Y
    ///,  
  "To determine if a normal toric variety is projective, we use the Gale dual
  vector configuration associated to the rays; see Theorem V.4.8 in Ewald's
  book ", EM "Combinatorial convexity and algebraic geometry", " for more
  informaiton.",
  SeeAlso => {
    "Basic invariants and properties of normal toric varieties",
    (isComplete,NormalToricVariety),    
    (isAmple,ToricDivisor)}}    
     
document {
  Key => {(fan,NormalToricVariety)},
  Headline => "make the 'Polyhedra' fan associated to the normal toric variety",
  Usage => "fan X",
  Inputs => {"X" => NormalToricVariety},
  Outputs => {Fan},
  "This methods returns the ", TO "Polyhedra", " fan associated to a normal
  toric variety.  More precisely, this method takes the set of ray listed in ",
  TT "max X", ", computes their positive hull as an object of class ", 
  TO "Polyhedra::Cone", " and returns the fan generated by these cones.",
  EXAMPLE lines ///
    PP3 = projectiveSpace 3;
    F = fan PP3
    rays F
    rays PP3
    ///,
  EXAMPLE lines ///
    FF2 = hirzebruchSurface 2;
    F = fan FF2
    rays F
    rays FF2
    ///,
  SeeAlso => {
    "Basic invariants and properties of normal toric varieties"}}
   
   
   
   
------------------------------------------------------------------------------
-- working with divisors
   
document { 
  Key => "Working with divisors and their associated groups",
  "The following methods allows one to make and manipulate torus-invariant
  Weil divisors on a normal toric variety.",
  SUBSECTION "Menu",
  UL {
    TO (toricDivisor,List,NormalToricVariety),
    TO (toricDivisor,NormalToricVariety),
    TO (symbol _, NormalToricVariety, ZZ),
    TO ToricDivisor,        
    TO (expression, ToricDivisor),
    TO (normalToricVariety, ToricDivisor),
    TO (support, ToricDivisor),
    TO (vector,ToricDivisor),
    TO (symbol +, ToricDivisor, ToricDivisor),
    TO (symbol SPACE, OO, ToricDivisor),
    TO (isEffective,ToricDivisor),
    TO (isCartier,ToricDivisor),    
    TO (isQQCartier,ToricDivisor), 
    TO (isNef, ToricDivisor),       
    TO (isAmple,ToricDivisor),
    TO (isVeryAmple,ToricDivisor),
    TO (vertices,ToricDivisor),
    TO (latticePoints,ToricDivisor),
    TO (polytope,ToricDivisor)},
  PARA{},
  "One can also work with the various groups arising from torus-invariant and
  the canonical maps between them.",
  SUBSECTION "Menu",
  UL {
    TO (wDiv,NormalToricVariety),
    TO (fromWDivToCl,NormalToricVariety),
    TO (cl,NormalToricVariety),
    TO (cDiv,NormalToricVariety),
    TO (fromCDivToWDiv,NormalToricVariety),
    TO (fromCDivToPic,NormalToricVariety),
    TO (pic,NormalToricVariety),
    TO (fromPicToCl,NormalToricVariety)},
  SeeAlso =>{
    "Making normal toric varieties",
    "Basic invariants and properties of normal toric varieties",
    "Total coordinate rings and coherent sheaves",
    "Resolution of singularities"}}

document { 
  Key => {wDiv, 
    (wDiv, NormalToricVariety)},
  Headline => "make the group of torus-invariant Weil divisors",
  Usage => "wDiv X",
  Inputs => {"X" => NormalToricVariety },
  Outputs => {Module => "a finitely generated free abelian group" },
  "The group of torus-invariant Weil divisors on a normal toric variety is the
  free abelian group generated by the torus-invariant prime divisors.  The prime
  divisors correspond to rays in the associated fan.  Since the rays are indexed
  in this package by ", TT "0, ..., n", ", the group of torus-invariant Weil
  divisors is canonically isomorphic to ", TEX ///$\ZZ^{n+1}$///, ".",
  PARA {},
  "The examples illustrate various possible Weil groups.",
  EXAMPLE lines ///
    PP2 = projectiveSpace 2;
    #rays PP2
    wDiv PP2
    ///,
  EXAMPLE lines ///
    FF7 = hirzebruchSurface 7;
    #rays FF7
    wDiv FF7
    ///,
  EXAMPLE lines ///
    U = normalToricVariety({{4,-1},{0,1}},{{0,1}});
    #rays U
    wDiv U
    ///,
  SeeAlso => {
    "Working with divisors and their associated groups",
    (fromCDivToWDiv,NormalToricVariety),
    (fromWDivToCl,NormalToricVariety),
    ToricDivisor,
    (vector,ToricDivisor)}}   

document { 
  Key => {fromWDivToCl, 
    (fromWDivToCl, NormalToricVariety)},
  Headline => "get the map from Weil divisors to the class group",
  Usage => "fromWDivToCl X",
  Inputs => {"X" => NormalToricVariety },
  Outputs => {Matrix => "defining the surjection from the torus-invariant Weil
     	                 divisors to the class group" },
  "For a normal toric variety, the class group has a presentation defined by the
  map from the group of torus-characters to group of torus-invariant Weil
  divisors induced by minimal nonzero lattice points on the rays of the
  associated fan.  Hence, there is a surjective map from the group of
  torus-invariant Weil divisors to the class group.  This method returns a
  matrix representing this map.  Since the ordering on the rays of the toric
  variety determines a basis for the group of torus-invariant Weil divisors,
  this matrix is determined by a choice of basis for the class group.",
  PARA{},
  "The examples illustrate some of the possible maps from the group of
  torus-invariant Weil divisors to the class group.",
  EXAMPLE lines ///
    PP2 = projectiveSpace 2;
    A = fromWDivToCl PP2
    source A == wDiv PP2
    target A == cl PP2
    ///,
  EXAMPLE lines ///
    X = weightedProjectiveSpace {1,2,2,3,4};
    fromWDivToCl X
    ///,	  
  EXAMPLE lines ///
    FF7 = hirzebruchSurface 7;
    A' = fromWDivToCl FF7
    (source A', target A') == (wDiv FF7, cl FF7)
    ///,
  EXAMPLE lines ///
    U = normalToricVariety({{4,-1},{0,1}},{{0,1}});
    fromWDivToCl U
    wDiv U
    cl U
    ///,
  "This matrix also induces the grading on the total coordinate ring of toric
  variety.",
  EXAMPLE lines ///
    degrees ring PP2
    degrees ring X
    degrees ring FF7
    ///,
  "The optional argument ", TO WeilToClass, " for the constructor ", 
  TO normalToricVariety, " allows one to specify a basis of the class group.",
  SeeAlso => {
    "Working with divisors and their associated groups",
    "Making normal toric varieties",
    (wDiv,NormalToricVariety),
    (cDiv,NormalToricVariety), 
    (ring,NormalToricVariety)}}   

document { 
  Key => {cl, 
    (cl, NormalToricVariety)},
  Headline => "make the class group",
  Usage => "cl X",
  Inputs => {"X" => NormalToricVariety},
  Outputs => {Module => "a finitely generated abelian group" },
  "The class group of a variety is the group of Weil divisors divided by the
  subgroup of principal divisors.  For a normal toric variety, the class group
  has a presentation defined by the map from the group of torus-characters to
  group of torus-invariant Weil divisors induced by minimal nonzero lattice
  points on the rays of the associated fan.",
  PARA {},
  "The following examples illustrate various possible class groups.",
  EXAMPLE lines ///
    cl projectiveSpace 2
    cl hirzebruchSurface 7
    AA3 = normalToricVariety({{1,0,0},{0,1,0},{0,0,1}},{{0,1,2}});
    cl AA3
    X = normalToricVariety({{4,-1},{0,1}},{{0,1}});
    cl X
    C = normalToricVariety({{1,0,0},{0,1,0},{0,0,1},{1,1,-1}},{{0,1,2,3}});
    cl C
    ///,
  "The total coordinate ring of a toric variety is graded by its class group.",
  SeeAlso => {
    "Working with divisors and their associated groups",
    (rays,NormalToricVariety), 
    (wDiv,NormalToricVariety),
    (ring, NormalToricVariety), 
    (fromPicToCl,NormalToricVariety),
    (fromWDivToCl,NormalToricVariety)}}   

document { 
  Key => {cDiv, 
    (cDiv, NormalToricVariety)},
  Headline => "make the group of torus-invariant Cartier divisors",
  Usage => "cDiv X",
  Inputs => {"X" => NormalToricVariety },
  Outputs => {Module => "a finitely generated abelian group" },
  "The group of torus-invariant Cartier divisors on ", TEX ///$X$///, " is the
  subgroup of all locally principal torus-invarient Weil divisors.  On a normal
  toric variety, the group of torus-invariant Cartier divisors can be computed
  as an inverse limit.  More precisely, if ", TEX ///$M$///, " denotes the
  lattice of characters on ", TEX ///$X$///, " and the maximal cones in the fan
  of ", TEX ///$X$///, " are ", TEX ///$s_0,\dots,s_{r-1}$///, " then we have ",
  TEX ///$CDiv(X) = ker( \oplus_{i} M/M(s_i{}) \to{} \oplus_{i<j} M/M(s_i \cap
  s_j{})$///, ".",
  PARA{},     
  "When ", TEX ///$X$///, " is smooth, every torus-invariant Weil divisor is
  Cartier.",
  EXAMPLE lines ///
    PP2 = projectiveSpace 2;
    Div = wDiv PP2
    Div == cDiv PP2
    id_Div == fromCDivToWDiv PP2
    isSmooth PP2
    ///,
  EXAMPLE lines ///
    FF1 = hirzebruchSurface 1;
    cDiv FF1
    isIsomorphism fromCDivToWDiv FF1
    isSmooth FF1
    ///,

  "On a simplicial toric variety, every torus-invariant Weil divisor is
  ", TEX ///$\QQ$///, "-Cartier --- every torus-invariant Weil divisor has a
  positive integer multiple that is Cartier.",
  
  EXAMPLE lines ///
    U = normalToricVariety({{4,-1},{0,1}},{{0,1}});
    cDiv U
    wDiv U
    fromCDivToWDiv U
    prune cokernel fromCDivToWDiv U
    isSimplicial U
    ///,
  EXAMPLE lines ///
    U' = normalToricVariety({{4,-1},{0,1}},{{0},{1}});
    cDiv U'
    wDiv U'
    fromCDivToWDiv U'
    isSmooth U'
    ///,     
  "In general, the Cartier divisors are only a subgroup of the Weil divisors.",
  EXAMPLE lines ///
    C = normalToricVariety({{1,0,0},{0,1,0},{0,0,1},{1,1,-1}},{{0,1,2,3}});
    cDiv C
    wDiv C
    prune coker fromCDivToWDiv C
    isSimplicial C
    ///,
  EXAMPLE lines ///	  
    X = normalToricVariety(id_(ZZ^3) | -id_(ZZ^3));
    wDiv X
    cDiv X
    prune cokernel fromCDivToWDiv X
    isSimplicial X
    ///,
  SeeAlso => {
    "Working with divisors and their associated groups",    
    (fromCDivToWDiv,NormalToricVariety),
    (fromCDivToPic,NormalToricVariety),
    (isCartier,ToricDivisor)}}   

document { 
  Key => {fromCDivToWDiv, 
    (fromCDivToWDiv, NormalToricVariety)},
  Headline => "get the map from Cartier divisors to Weil divisors",
  Usage => "fromCDivToWDiv X",
  Inputs => {"X" => NormalToricVariety },
  Outputs => {Matrix => " the inclusion map from the group of torus-invariant
	       Cartier divisors to the group of torus-invariant Weil divisors" },
  "The group of torus-invariant Cartier divisors is the subgroup of all locally
  principal torus-invariant Weil divisors.",
  PARA{},	  
  "On a smooth normal toric variety, every torus-invariant Weil divisor is
  Cartier, so the inclusion map is simply the identity map.",
  EXAMPLE lines ///
    PP2 = projectiveSpace 2;
    cDiv PP2
    fromCDivToWDiv PP2
    isSmooth PP2
    ///,
  EXAMPLE lines ///
    FF1 = hirzebruchSurface 1;
    cDiv FF1
    fromCDivToWDiv FF1
    isSmooth FF1
    ///,
  "On a simplicial normal toric variety, every torus-invariant Weil divisor is
  ", TEX ///$\QQ$///, "-Cartier --- every torus-invariant Weil divisor has a
  positive integer multiple that is Cartier.",
  EXAMPLE lines ///
    U = normalToricVariety({{4,-1},{0,1}},{{0,1}});
    cDiv U
    wDiv U
    fromCDivToWDiv U
    prune cokernel fromCDivToWDiv U
    isSimplicial U
    ///,
  EXAMPLE lines ///
    U' = normalToricVariety({{4,-1},{0,1}},{{0},{1}});
    cDiv U'
    wDiv U'
    fromCDivToWDiv U'
    isSmooth U'
    ///,     
  "In general, the Cartier divisors are only a subgroup of the Weil divisors.",
  EXAMPLE lines ///
    C = normalToricVariety({{1,0,0},{0,1,0},{0,0,1},{1,1,-1}},{{0,1,2,3}});
    cDiv C
    wDiv C
    fromCDivToWDiv C
    prune coker fromCDivToWDiv C
    isSimplicial C
    ///,
  EXAMPLE lines ///
    X = normalToricVariety(id_(ZZ^3) | -id_(ZZ^3));
    wDiv X
    cDiv X
    fromCDivToWDiv X
    prune cokernel fromCDivToWDiv X
    isSimplicial X
    ///,
  SeeAlso => {
    "Working with divisors and their associated groups",    
    (wDiv,NormalToricVariety), 
    (cDiv,NormalToricVariety),
    (isCartier,ToricDivisor)}}   

document { 
  Key => {fromCDivToPic, 
    (fromCDivToPic, NormalToricVariety)},
  Headline => "get the map from Cartier divisors to the Picard group",
  Usage => "fromCDivToPic X",
  Inputs => {"X" => NormalToricVariety },
  Outputs => {Matrix => " the surjective map from the group of torus-invariant
                        Cartier divisors to the Picard group" },
  "The Picard group of a variety is the group of Cartier divisors divided by 
  the subgroup of principal divisors.  For a normal toric variety , the 
  Picard group has a presentation defined by the map from the group of 
  torus-characters to the group of torus-invariant Cartier divisors.  Hence, 
  there is a surjective map from the group of torus-invariant Cartier 
  divisors to the Picard group.  This function returns a matrix representing 
  this map.",
  PARA{},	  
  "On a smooth normal toric variety, the map from the torus-invariant Cartier
  divisors to the Picard group is the same as the map from the Weil divisors 
  to the class group.",
  EXAMPLE lines ///
    PP2 = projectiveSpace 2;
    eta = fromCDivToPic PP2
    eta == fromWDivToCl PP2
    ///,
  EXAMPLE lines ///
    FF1 = hirzebruchSurface 1;
    xi = fromCDivToPic FF1
    xi == fromWDivToCl FF1
    ///,
  "In general, there is a commutative diagram relating the map from the group 
  of torus-invariant Cartier divisors to the Picard group and the map from the
  group of torus-invariant Weil divisors to the class group.",
  EXAMPLE lines ///
    C = normalToricVariety({{1,0,0},{0,1,0},{0,0,1},{1,1,-1}},{{0,1,2,3}});
    fromCDivToPic C
    pic C	  
    fromWDivToCl C
    fromCDivToWDiv C
    fromPicToCl C
    fromWDivToCl C * fromCDivToWDiv C == fromPicToCl C * fromCDivToPic C
    ///,
  EXAMPLE lines ///
    X = normalToricVariety(id_(ZZ^3) | -id_(ZZ^3));
    fromCDivToPic X
    pic X
    fromWDivToCl X
    fromCDivToWDiv X
    fromPicToCl X
    fromWDivToCl X * fromCDivToWDiv X == fromPicToCl X * fromCDivToPic X
    ///,
  SeeAlso => {
    "Working with divisors and their associated groups",        
    (cDiv,NormalToricVariety),
    (pic,NormalToricVariety),
    (fromWDivToCl,NormalToricVariety)}}   

document { 
  Key => {pic, 
    (pic,NormalToricVariety)},
  Headline => "make the Picard group",
  Usage => "pic X",
  Inputs => {"X" => NormalToricVariety},
  Outputs => {Module => " a finitely generated abelian group"},
  "The Picard group of a variety is the group of Cartier divisors divided by 
  the subgroup of principal divisors.  For a normal toric variety, the Picard 
  group has a presentation defined by the map from the group of 
  torus-characters to the group of torus-invariant Cartier divisors.",
  PARA {},
  "When toric variety is smooth, the Picard group is isomorphic to the class
  group.",
  EXAMPLE lines ///
    PP3 = projectiveSpace 3;
    pic PP3
    cl PP3
    ///,
  EXAMPLE lines ///
    FF7 = hirzebruchSurface 7;
    pic FF7 == cl FF7
    ///,
  "For an affine toric variety, the Picard group is trivial.",
  EXAMPLE lines ///
    U = normalToricVariety({{4,-1},{0,1}},{{0,1}});
    pic U
    cl U
    ///,
  EXAMPLE lines ///
    U' = normalToricVariety({{4,-1},{0,1}},{{0},{1}});
    pic U'
    cl U'
    ///,
  "If the fan associated to ", TEX ///$X$///, " contains a cone of 
  dimension ", TEX ///$dim(X)$///, " then the Picard group is free.",
  EXAMPLE lines ///
    C = normalToricVariety({{1,0,0},{0,1,0},{0,0,1},{1,1,-1}},{{0,1,2,3}});
    pic C
    cl C
    ///,
  EXAMPLE lines ///
    X = normalToricVariety(id_(ZZ^3) | -id_(ZZ^3));
    pic X
    cl X
    ///,
  SeeAlso => {
    "Working with divisors and their associated groups", 
    (cl,NormalToricVariety),
    (cDiv,NormalToricVariety),
    (fromCDivToPic,NormalToricVariety),
    (fromPicToCl,NormalToricVariety)}}     

document { 
  Key => {fromPicToCl, 
    (fromPicToCl,NormalToricVariety)},
  Headline => "get the map from Picard group to class group",
  Usage => "fromPicToCl X",
  Inputs => {"X" => NormalToricVariety},
  Outputs => {Matrix => " the inclusion map from the Picard group to the class
	                group"},
  "The Picard group of a normal toric variety is a subgroup of the class
  group.",
  PARA{},
  "On a smooth normal toric variety, the Picard group is isomorphic to the 
  class group, so the inclusion map is the identity.",
  EXAMPLE lines ///
    PP3 = projectiveSpace 3;
    pic PP3
    cl PP3
    fromPicToCl PP3
    ///,
  EXAMPLE lines ///
    FF7 = hirzebruchSurface 7;
    pic FF7 == cl FF7
    fromPicToCl FF7
    ///,
  "For weighted projective space, the inclusion corresponds to ", TEX ///$l
  \ZZ$///, " in ", TEX ///$\ZZ$///, ", where ", TEX ///$l = lcm(q_0,\dots, q_d
  {})$///, ".",
  EXAMPLE lines ///
    X = weightedProjectiveSpace {1,2,3};
    pic X
    cl X
    fromPicToCl X
    ///,
  EXAMPLE lines ///
    Y = weightedProjectiveSpace {1,2,2,3,4};
    pic Y
    cl Y
    fromPicToCl Y
    ///,
  "The following examples illustrate some other possibilities.",
  EXAMPLE lines ///
    C = normalToricVariety({{1,0,0},{0,1,0},{0,0,1},{1,1,-1}},{{0,1,2,3}});
    pic C
    cl C
    fromPicToCl C
    ///,
  EXAMPLE lines ///
    X = normalToricVariety(id_(ZZ^3) | - id_(ZZ^3));
    rays X
    max X
    pic X
    cl X
    fromPicToCl X
    prune cokernel fromPicToCl X
    ///,
  SeeAlso => {
    "Working with divisors and their associated groups", 
    (pic,NormalToricVariety), 
    (cl,NormalToricVariety)}}     

document {
  Key => ToricDivisor,
  Headline => "the class of all torus-invariant Weil divisors",
  "A torus-invariant Weil divisor on a normal toric variety is an integral
  linear combination of the torus-invariant prime divisors.  The 
  torus-invariant prime divisors correspond to the rays.  In this package, 
  the rays are ordered and indexed by the nonnegative integers.",
  PARA{},
  "The first examples illustrates some torus-invariant Weil divisors on
  projective 2-space",
  EXAMPLE lines ///
    PP2 = projectiveSpace 2;
    D1 = toricDivisor({2,-7,3},PP2) 
    D2 = 2*PP2_0 + 4*PP2_2
    D1+D2
    D1-D2
    K = toricDivisor PP2  
    ///,
  "One can easily extract individual coefficients or the vector of 
  coefficients",
  EXAMPLE lines ///
    D1#0
    D1#1
    D1#2
    vector D1
    vector K
    ///,
  SeeAlso => {
    "Working with divisors and their associated groups",
    (wDiv,NormalToricVariety),
    (toricDivisor,List,NormalToricVariety),    
    (toricDivisor,NormalToricVariety),
    (symbol _, NormalToricVariety, ZZ),
    (expression, ToricDivisor),
    (normalToricVariety, ToricDivisor),
    (support, ToricDivisor),
    (vector,ToricDivisor),
    (symbol +, ToricDivisor, ToricDivisor)}}

document {
  Key => {(expression, ToricDivisor)},
  Headline => "get the expression used to format for printing",
  Usage => "expression D",
  Inputs => {"D" => ToricDivisor},
  Outputs => {Expression => {" used to format ", TT "D", " for printing"}},
  "This method is the primary function called upon by ", TO(symbol <<), " to
  format for printing.",
  PARA{},
  "In this package, ", TEX ///$i$///, "-th torus-invariant prime Weil divisors
  are displayed as ", TEX ///D_i///, ".  Hence, an arbitrary torus-invariant 
  Weil divisor is displayed as an integral linear combination of these 
  expressions.",
  EXAMPLE lines ///
    PP2 = projectiveSpace 2;
    toricDivisor({2,-7,3},PP2)
    2*PP2_0+4*PP2_2
    toricDivisor PP2
    ///,
  SeeAlso => { 
    "Working with divisors and their associated groups",
    (symbol _, NormalToricVariety, ZZ),    
    ToricDivisor}}

undocumented { (net,ToricDivisor), emsBound, rawHHOO }

document {
  Key => {(vector, ToricDivisor)},
  Headline => "make the vector of coefficients",
  Usage => "vector D",
  Inputs => {"D" => ToricDivisor},
  Outputs => {Vector => "of integer coefficients"},
  "This method returns the vector whose ", TT "i", "-th entry is the 
  coefficient of ", TT "i", "-th torus-invariant prime divisor.  The indexing 
  of the torus-invariant prime divisors is inherited from the indexing of the 
  rays in the associated fan.  This vector can be viewed as an element of the 
  group of torus-invariant Weil divisors.",
  EXAMPLE lines ///
    PP2 = projectiveSpace 2;
    D = 2*PP2_0 - 7*PP2_1 + 3*PP2_2    
    vector D
    wDiv PP2
    ///,
  EXAMPLE lines ///
    FF7 = hirzebruchSurface 7;
    E = FF7_0-5*FF7_3
    vector E
    wDiv FF7
    ///,
  SeeAlso => { 
    "Working with divisors and their associated groups",
    ToricDivisor,
    (rays,NormalToricVariety),
    (wDiv,NormalToricVariety)}}

document {
  Key => {(support, ToricDivisor)},
  Headline => "make the list of prime divisors with nonzero coefficients",
  Usage => "support D",
  Inputs => {"D" => ToricDivisor},
  Outputs => {List => {"indexing the torus-invariant prime divisors with 
                        nonzero coefficients"}},
  "The support of a torus-invariant Weil divisor is the set of torus-invariant
  prime divisors which appear with nonzero coefficients in the unique 
  expression for this divisor.  In this package, we encode this information 
  by indexing the torus-invariant prime divisors with appear with a nonzero 
  coefficient.  The indexing of the torus-invariant prime divisors is 
  inherited from the indexing of the rays in the associated fan.",
  EXAMPLE lines ///
    PP2 = projectiveSpace 2;
    D = 2*PP2_0 - 7*PP2_1 + 3*PP2_2    
    support D
    E = PP2_0-5*PP2_2
    support E
    support (6*PP2_1)
    ///,
  SeeAlso => { 
    "Working with divisors and their associated groups",
    (rays,NormalToricVariety),    
    ToricDivisor}}

document {
  Key => {(variety,ToricDivisor),
    (normalToricVariety,ToricDivisor)},
  Headline => "get the underlying normal toric variety",
  Usage => "variety D or normalToricVariety D",
  Inputs => {"D" => ToricDivisor},
  Outputs => {NormalToricVariety => "the underlying variety"},
  "This method allows one to easily access the normal toric variety on which 
  the torus-invariant Weil divisor is defined.",
  EXAMPLE lines ///
    PP2 = projectiveSpace 2;
    D = 2*PP2_0 - 7*PP2_1 + 3*PP2_2
    variety D
    normalToricVariety D
    ///,    
  EXAMPLE lines ///
    X = normalToricVariety(id_(ZZ^3) | - id_(ZZ^3));
    E = X_0-5*X_3
    X === variety E
    X === normalToricVariety E
    ///,     
  SeeAlso => { 
    "Working with divisors and their associated groups"}}

document {
  Key => {toricDivisor,
    (toricDivisor,List,NormalToricVariety)},
  Headline => "make a torus-invariant Weil divisor",
  Usage => "toricDivisor(L,X)",
  Inputs => {
    "L" => List => "of integers",
    "X" => NormalToricVariety},
  Outputs => {ToricDivisor},
  "Given a list of integers and a normal toric variety, this method returns 
  the torus-invariant Weil divisor such the coefficient of the ", TT "i", 
  "-th torus-invariant prime divisor is the ", TT "i", "-th entry in the 
  list.  The indexing of the torus-invariant prime divisors is inherited from 
  the indexing of the rays in the associated fan.  In this package, the rays 
  are ordered and indexed by the nonnegative integers.",
  EXAMPLE lines ///
    PP2 = projectiveSpace 2;
    D = toricDivisor({2,-7,3},PP2)
    D === 2* PP2_0 - 7*PP2_1 + 3*PP2_2
    vector D
    ///,     
  "Although this is a general method for making a torus-invariant Weil 
  divisor, it is typically more convenient to simple enter the appropriate 
  linear combination of torus-invariant Weil divisors.",
  SeeAlso => { 
    "Working with divisors and their associated groups",
    (toricDivisor,NormalToricVariety),
    (symbol _, NormalToricVariety, ZZ)}}

document {
  Key => {(toricDivisor,NormalToricVariety)},
  Headline => "make the canonical divisor",
  Usage => "toricDivisor X",
  Inputs => {"X" => NormalToricVariety},
  Outputs => {ToricDivisor => " minus the sum of all the torus-invariant prime
                               divisors"},
  "On a smooth normal toric variety, the canonical divisor equals minus the 
  sum of all the torus-invariant prime divisors.  For a singular toric 
  variety, this divisor may not be Cartier or even ", TT "QQ", "-Cartier.  
  Nevertheless, the associated coherent sheaf, whose local sections are 
  rational functions with at least simple zeros along the prime divisors, is 
  the dualizing sheaf.",
  PARA{},
  "The first example illustrates the canonical divisor on projective space",
  EXAMPLE lines ///
    PP3 = projectiveSpace 3;
    K = toricDivisor PP3
    omega = OO K
    HH^3(PP3, OO_PP3(-7) ** omega)
    HH^0(PP3, OO_PP3(7))
    ///, 
  "The second example illustrates that duality also holds on complete singular
  nonprojective toric varieties.",
  EXAMPLE lines ///
    Rho = {{-1,-1,1},{3,-1,1},{0,0,1},{1,0,1},{0,1,1},{-1,3,1},{0,0,-1}};
    Sigma = {{0,1,3},{0,1,6},{0,2,3},{0,2,5},{0,5,6},{1,3,4},{1,4,5},{1,5,6},{2,3,4},{2,4,5}};
    X = normalToricVariety(Rho,Sigma);
    isSmooth X
    isComplete X    
    isProjective X
    K = toricDivisor X    
    isCartier K
    omega = OO K
    HH^0(X, OO_X(-1,2,4,5))
    HH^3(X, OO_X(1,-2,-4,-5) ** omega)
    ///,         
  SeeAlso => { 
    "Working with divisors and their associated groups",
    toricDivisor, 
    (symbol SPACE, OO, ToricDivisor),
    (cohomology,ZZ,NormalToricVariety,CoherentSheaf)}}

document {
  Key => {(symbol _, NormalToricVariety, ZZ)},
  Headline => "make a torus-invariant prime divisor",
  Usage => "X_i",
  Inputs => {
    "X" => NormalToricVariety,
    "i" => ZZ},
  Outputs => {ToricDivisor => {" the torus-invariant prime divisor associated 
                                to the ", TT "i", "-th ray"}},
  "The torus-invariant prime divisors on a normal toric variety correspond to
  the rays in the associated fan.  In this package, the rays are ordered and
  indexed by the nonnegative integers.  Given a normal toric variety and
  nonnegative integer, this method returns the corresponding torus-invariant
  prime divisor.  The most convenient way to make a general torus-invariant 
  Weil divisor is to simply write the appropriate linear combination of these
  torus-invariant Weil divisors.",
  PARA{},  
  "There are three torus-invariant prime divisors in the projective plane.",
  EXAMPLE lines ///
    PP2 = projectiveSpace 2;
    PP2_0
    PP2_1
    PP2_2
    - PP2_0 - PP2_1 - PP2_2 === toricDivisor PP2 
    ///,
  "A torus-invariant Weil divisor is prime if and only if its support has a
  single element.",
  EXAMPLE lines ///
    X = normalToricVariety(id_(ZZ^3) | -id_(ZZ^3));
    X_0
    #support X_0
    K = toricDivisor X
    #support toricDivisor X
    ///,
  SeeAlso => { 
    "Working with divisors and their associated groups",
    toricDivisor, 
    (variety,ToricDivisor)}}

document {
  Key => {(symbol +, ToricDivisor, ToricDivisor),
    (symbol -, ToricDivisor, ToricDivisor),
    (symbol -, ToricDivisor),
    (symbol *, ZZ, ToricDivisor)},
  Headline => "arithmetic of toric divisors",
  Usage => "D + E",
  Inputs => {
    "D" => ToricDivisor,
    "E" => ToricDivisor},
  Outputs => {ToricDivisor},
  "The set of torus-invariant Weil divisors forms an abelian group under
  addition.  The basic operations arising from this structure, including
  addition, substraction, negation, and scalar multplication by integers, are
  available.",
  EXAMPLE lines ///
    X = normalToricVariety(id_(ZZ^3) | -id_(ZZ^3));
    #rays X
    D = toricDivisor({2,-7,3,0,7,5,8,-8},X)
    K = toricDivisor X
    D+K
    D+K === K+D
    D-K
    D-K === -(K-D)
    -K
    -K === (-1)*K
    7*D
    7*D === (3+4)*D
    -3*D+7*K    
    -3*D+7*K === (-2*D+8*K) + (-D-K)
    ///,
  SeeAlso => { 
    "Working with divisors and their associated groups"}}

document {
  Key => {(symbol SPACE, OO,ToricDivisor)},
  Headline => "make the associated rank-one reflexive sheaf",
  Usage => "OO D",
  Inputs => {"D" => ToricDivisor},
  Outputs => {CoherentSheaf => " the associated rank-one reflexive sheaf"},
  "For a Weil divisor ", TEX ///$D$///, " on a normal variety ", 
  TEX ///$X$///, " the associated sheaf ", TEX ///${\cal O}_X(D)$///, 
  " is defined by ", TEX ///$H^0(U, {\cal O}_X(D)) = 
  \{ f \in {\mathbb C}(X)^* | (div(f)+D)|_U \geq 0 \} \cup \{0\}$///, ".  The 
  sheaf associated to a Weil divisor is reflexive; it is equal to its 
  bidual.  A divisor is Cartier if and only if the associated sheaf is
  a line bundle.",
  PARA{},
  "The first examples show that the associated sheaves are reflexive.",
  EXAMPLE lines ///
    PP3 = projectiveSpace 3;
    K = toricDivisor PP3
    omega = OO K
    omegaVee = prune sheafHom(omega, OO_PP3)
    omega === prune sheafHom(omegaVee, OO_PP3)
    ///,
  EXAMPLE lines ///
    X = hirzebruchSurface 2;
    D = X_0+X_1
    L = OO D
    LVee = prune sheafHom(L, OO_X)
    L === prune sheafHom(LVee, OO_X)
    ///,
  EXAMPLE lines ///    
    Rho = {{1,0,0},{0,1,0},{0,0,1},{0,-1,-1},{-1,0,-1},{-2,-1,0}};
    Sigma = {{0,1,2},{0,1,3},{1,3,4},{1,2,4},{2,4,5},{0,2,5},{0,3,5},{3,4,5}};
    Y = normalToricVariety(Rho,Sigma);
    isSmooth Y
    isProjective Y
    E = Y_0 + Y_2 + Y_4
    isCartier E
    F = OO E
    FVee = prune sheafHom(F, OO_Y)
    F === prune sheafHom(FVee, OO_Y)
    ///,     
  "Two Weil divisors ", TEX ///$D$///, " and ", TEX ///$E$///, " are linearly
  equivalent if ", TEX ///$D = E + div(f)$///, " for some ", 
  TEX ///$f \in {\mathbb C}(X)^*$///, ".  Linearly equivalent divisors 
  produce isomorphic sheaves.",
  EXAMPLE lines ///
    PP3 = projectiveSpace 3;
    D1 = PP3_0
    E1 = PP3_1
    OO D1 === OO E1
    X = hirzebruchSurface 2;
    D2 = X_2+X_3    
    E2 = 3*X_0+X_1
    OO D2 === OO E2
    ///,    
  SeeAlso => { 
    "Working with divisors and their associated groups",
    "Total coordinate rings and coherent sheaves"}}

document {
  Key => {isEffective, (isEffective,ToricDivisor)},
  Headline => "whether a torus-invariant Weil divisor is effective",
  Usage => "isEffective D",
  Inputs => {"D" => ToricDivisor},
  Outputs => {Boolean => {TO "true", " if all the coefficients of the
                          torus-invariant prime divisors are nonnegative, 
			  and ", TO "false", " otherwise"}},
  "A torus-invariant Weil divisor is effective if all the coefficients of the
  torus-invariant prime divisors are nonnegative.",
  PARA{},
  "The canonical divisor is not effective, but the anticanonical divisor is.",
  EXAMPLE lines ///
    PP3 = projectiveSpace 3;
    K = toricDivisor PP3
    isEffective K
    isEffective(-K)
    ///,
  "The torus-invariant prime divisors generate the cone of effective 
  divisors.",
  SeeAlso => { 
    "Working with divisors and their associated groups"}}   

document {
  Key => {isCartier, (isCartier,ToricDivisor)},
  Headline => "whether a torus-invariant Weil divisor is Cartier",
  Usage => "isCartier D",
  Inputs => {"D" => ToricDivisor},
  Outputs => {Boolean => {TO "true", " if the divisor is Cartier, and ", 
	      TO "false", " otherwise" }},
  "A torus-invariant Weil divisor ", TEX ///$D$///, " on a normal toric 
  variety ", TEX ///$X$///, " is Cartier if it is locally principal, meaning 
  that ", TEX ///$X$///, " has an open cover ", TEX ///$\{U_i\}$///, " such 
  that ", TEX ///$D|_{U_i}$///, " is principal in ", TEX ///$U_i$///, " for 
  every ", TEX ///$i$///, ".",
  PARA{},
  "On a smooth variety, every Weil divisor is Cartier.",
  EXAMPLE lines ///
    PP3 = projectiveSpace 3;
    all(3, i -> isCartier PP3_i)
    ///,
  "On a simplicial toric variety, every torus-invariant Weil divisor is ", 
  TEX ///$\QQ$///, "-Cartier --- every torus-invariant Weil divisor has a
  positive integer multiple that is Cartier.",
  EXAMPLE lines ///
    W = weightedProjectiveSpace {2,5,7};
    isSimplicial W
    isCartier W_0    
    isQQCartier W_0
    isCartier (35*W_0)      
    ///,
  "In general, the Cartier divisors are only a subgroup of the Weil 
  divisors.",
  EXAMPLE lines ///
    X = normalToricVariety(id_(ZZ^3) | -id_(ZZ^3));
    isCartier X_0
    isQQCartier X_0
    K = toricDivisor X
    isCartier K
    ///,  
  SeeAlso => { 
    "Working with divisors and their associated groups",
    (cDiv,NormalToricVariety),
    (isSimplicial,NormalToricVariety),
    (isQQCartier,ToricDivisor)}}  

document {
  Key => {isQQCartier, (isQQCartier,ToricDivisor)},
  Headline => "whether a torus-invariant Weil divisor is QQ-Cartier",
  Usage => "isCartier D",
  Inputs => {"D" => ToricDivisor},
  Outputs => {Boolean => {TO "true", " if the divisor is ", TEX ///$\QQ$///,
              "-Cartier, and ", TO "false", " otherwise"}},
  "A Weil divisor is ", TEX ///$\QQ$///, "-Cartier if some positive integer
  multiple is Cartier.",
  PARA{},
  "On a simplicial toric variety, every torus-invariant Weil divisor is ", 
  TEX ///$\QQ$///, "-Cartier.",
  EXAMPLE lines ///
    W = weightedProjectiveSpace {2,5,7};
    isSimplicial W
    isCartier W_0    
    isQQCartier W_0
    isCartier (35*W_0)      
    ///,
  "In general, the ", TEX ///$\QQ$///, "-Cartier divisors form a proper 
  subgroup of the Weil divisors.",
  EXAMPLE lines ///
    X = normalToricVariety(id_(ZZ^3) | -id_(ZZ^3));
    isCartier X_0
    isQQCartier X_0
    K = toricDivisor X
    isCartier K
    ///,    
  SeeAlso => { 
    "Working with divisors and their associated groups",
    (cDiv,NormalToricVariety),
    (isSimplicial,NormalToricVariety),
    (isCartier,ToricDivisor)}}   

document {
  Key => {isNef, (isNef,ToricDivisor)},
  Headline => "whether a torus-invariant Weil divisor is nef",
  Usage => "isNef D",
  Inputs => {"D" => ToricDivisor},
  Outputs => {Boolean => {TO "true", " if the divisor is nef, and ",
                          TO "false", " otherwise"}},
  "A ", TEX ///$\QQ$///, "-Cartier divisor is nef (short for \"numerically
  effective\" or \"numerically eventually free\") if the intersection product 
  of the divisor with every complete irreducible curve is nonnegative.  The
  definition depends only on the numerical equivalence class of the divisor.
  For a torus-invariant", TEX ///$\QQ$///, "-Cartier divisor ", 
  TEX ///$D$///, " on a complete normal toric variety, the following are 
  equivalent:",
  UL { 
    {TEX ///$D$///, " is nef;"},
    {"some positive integer multiply of ", TEX ///$D$///, " is Cartier and
    basepoint free;"},
    {"the real piecewise linear support function associated to ", 
      TEX ///$D$///, " is convex."}},
  "A torus-invariant Cartier divisor is nef if and only if it is basepoint 
  free; in other words, the associated line bundle is generated by its global
  sections.",
  PARA{},
  "On a Hirzebruch surface, three of the four torus-invariant prime divisors 
  are nef.",
  EXAMPLE lines ///
    X1 = hirzebruchSurface 2;
    isNef X1_0
    isAmple X1_0
    isNef X1_1    
    isNef X1_2
    isAmple X1_2
    isNef X1_3
    isAmple X1_3
    ///,
  "Not every ", TEX ///$\QQ$///, "-Cartier nef divisor is basepoint free.",
  EXAMPLE lines ///
    X2 = weightedProjectiveSpace {2,3,5}
    D = X2_1-X2_0
    isNef D
    HH^0(X2, OO D)
    for i from 1 to dim X2 list HH^i(X2, OO D)
    isCartier D    
    isCartier (30*D)
    HH^0(X2, OO (30*D))
    for i from 1 to dim X2 list HH^i(X2, OO (30*D))
    ///,
  "There are smooth complete normal toric varieties with no nontrivial nef
  divisors.",
  EXAMPLE lines ///
    R2 = {{1,0,0},{0,1,0},{0,0,1},{0,-1,2},{0,0,-1},{-1,1,-1},{-1,0,-1},{-1,-1,0}};
    S2 = {{0,1,2},{0,2,3},{0,3,4},{0,4,5},{0,1,5},{1,2,7},{2,3,7},{3,4,7},{4,5,6},{4,6,7},{5,6,7},{1,5,7}};  
    X3 = normalToricVariety(R2,S2);    
    isComplete X3
    isProjective X3 
    isSmooth X3
    any(#rays X3, i -> isNef X3_i)
    isNef (0*X3_1)    
    ///,
  "The most basic vanishing theorem for normal toric varieties states that the
  higher cohomology of coherent sheaf associated to a nef divisor is zero.",
  EXAMPLE lines ///
    X4 = kleinschmidt(9,{1,2,3});
    isNef X4_0
    isAmple X4_0
    for i from 1 to dim X4 list HH^i(X4, OO X4_0)
    D = X4_0+X4_4
    isNef D
    isAmple D
    for i from 1 to dim X4 list HH^i(X4, OO D)
    ///,
  SeeAlso => { 
    "Working with divisors and their associated groups",
    (isComplete,NormalToricVariety),
    (kleinschmidt,ZZ,List),
    (isQQCartier,ToricDivisor),
    (isAmple,ToricDivisor)}} 

document {
  Key => {isAmple, (isAmple,ToricDivisor)},
  Headline => "whether a torus-invariant Weil divisor is ample",
  Usage => "isAmple D",
  Inputs => {"D" => ToricDivisor},
  Outputs => {Boolean => {TO "true", " if the divisor is ample, and ",
                          TO "false", " otherwise"}},
  "A Cartier divisor is very ample when it is basepoint free and the map 
  arising from its complete linear series is a closed embedding.  A Cartier 
  divisor is ample when some positive integer multiple is very ample.  For a
  torus-invariant Cartier divisor ", TEX ///$D$///, " on a complete normal 
  toric variety, the following conditions are equivalent: ",
  UL {
    {TEX ///$D$///, " is ample;"},
    {"the real piecewise linear support function associated to ", 
      TEX ///$D$///, " is strictly convex;"},
    {"the lattice polytope corresponding to ", TEX ///$D$///, " is
    full-dimensional and its normal fan equals the fan associated to the
    underlying toric variety;"},
    {"the intersection product of ", TEX ///$D$///, " with every 
      torus-invariant irreducible curve is positive."}},
  PARA{},
  "On projective space, every torus-invariant prime divisor is ample.",
  EXAMPLE lines ///
    PP3 = projectiveSpace 3;
    all(#rays PP3, i -> isAmple PP3_i)
    ///,
  "On a Hirzebruch surface, none of the torus-invariant prime divisors are
  ample.",
  EXAMPLE lines ///
    X1 = hirzebruchSurface 2;
    any(#rays X1, i -> isAmple X1_i)
    D = X1_2 + X1_3
    isAmple D  
    isProjective X1     
    ///,
  "A normal toric variety is Fano if and only if its anticanonical divisors,
  namely minus the sum of its torus-invariant prime divisors, is ample.",
  EXAMPLE lines ///
    X2 = smoothFanoToricVariety(3,5);
    K = toricDivisor X2
    isAmple (- K)
    X3 = kleinschmidt(9,{1,2,3});
    K = toricDivisor X3
    isAmple (-K)  
    ///,      
  SeeAlso => { 
    "Working with divisors and their associated groups",
    (isComplete,NormalToricVariety),
    (isProjective,NormalToricVariety),
    (isNef,ToricDivisor),
    (isFano,NormalToricVariety)}} 

document {
  Key => {(isVeryAmple,ToricDivisor)},
  Headline => "whether a torus-invariant Weil divisor is very ample",
  Usage => "isVeryAmple D",
  Inputs => {"D" => ToricDivisor},
  Outputs => {Boolean => {TO "true", " if the divisor is very ample, and ", 
	                  TO "false", " otherwise" }},
  "A Cartier divisor is very ample when it is basepoint free and the map 
  arising from its complete linear series is a closed embedding.  On a normal 
  toric variety, the following are equivalent:",
  UL {
    {TEX ///$D$///, " is a very ample divisor;"},
    {"for the associated lattice polytope ", TEX ///$P$///, " and every vertex
    ", TEX ///$m_i \in P$///, ", the semigroup ", TEX ///$\NN(P \cap M -
    m_i)$///, " is saturated in the group characters ", TEX ///$M$///, "."}},
  PARA {},
  "On a smooth normal toric variety every ample divisor is very ample.",
  EXAMPLE lines ///
    PP3 = projectiveSpace 3;
    isAmple PP3_0
    isVeryAmple PP3_0
    FF2 = hirzebruchSurface 2;
    isAmple (FF2_2+FF2_3)
    isVeryAmple (FF2_2+FF2_3)
    ///,
  "A Cartier divisor is ample when some positive integer multiple is very
  ample.  On a normal toric variety of dimension ", TEX ///$d$///, " the ",
  TEX ///$(d-1)$///, " multiple of any ample divisor is always very ample.",
  
  EXAMPLE lines ///
    X = normalToricVariety matrix {{0,1,0,0,1},{0,0,1,0,1},{0,0,0,1,1},{0,0,0,0,3}};
    dim X
    D = 3*X_0
    isAmple D
    isVeryAmple D
    isVeryAmple (2*D)
    isVeryAmple (3*D)    
    ///,
  SeeAlso => { 
    "Working with divisors and their associated groups",
    (isProjective,NormalToricVariety),
    (isAmple,ToricDivisor)}} 

document {
  Key => {(vertices,ToricDivisor)},
  Headline => "computes the vertices of the associated polytope",
  Usage => "vertices D",
  Inputs => {"D" => ToricDivisor},
  Outputs => {Matrix => " whose columns are the vertices of the associated
              polytope"},
  "On a complete normal toric variety, the polyhedron associated to a Cartier
  divisor is a lattice polytope.  Given a torus-invariant Cartier divisor on a
  normal toric variety, this method returns an integer matrix whose columns
  correspond to the vertices of the associated lattice polytope.  For a
  non-effective Cartier divisor, this methods returns ", TO null, ".  When the
  divisor is ample, the normal fan the corresponding polytope equals the fan
  associated to the normal toric variety.",
  PARA {},
  "On the projective plane, the associate polytope is either empty, a point, 
  or a triangle.",
  EXAMPLE lines ///
    PP2 = projectiveSpace 2;
    vertices (-PP2_0)
    null === vertices (- PP2_0)
    vertices (0*PP2_0)
    isAmple PP2_0
    V1 = vertices (PP2_0)
    X1 = normalToricVariety V1;
    set rays X1 === set rays PP2
    max X1 === max PP2
    isAmple (2*PP2_0)
    V2 = vertices (2*PP2_0)
    X2 = normalToricVariety V2;
    rays X2 === rays X1
    max X2 === max X1
    ///,
  "On a Hirzebruch surface, the polytopes associated to non-ample Cartier
  divisors give rise to other normal toric varieties.",
  EXAMPLE lines ///
    FF2 = hirzebruchSurface 2;
    isAmple FF2_2
    V3 = vertices FF2_2
    normalToricVariety V3  -- a degenerated version of the projective line
    isDegenerate normalToricVariety V3  
    isAmple FF2_3
    V4 = vertices FF2_3
    normalToricVariety V4 -- a weighted projective space
    vertices FF2_1
    isAmple (FF2_2+FF2_3)
    V5 = vertices (FF2_2+FF2_3)
    normalToricVariety V5 -- isomorphic Hirzebruch surface
    ///,
  SeeAlso => { 
    "Working with divisors and their associated groups",
    (isComplete,NormalToricVariety),
    (isCartier,ToricDivisor),
    (isEffective,ToricDivisor),
    (normalToricVariety,Matrix),
    (latticePoints,ToricDivisor)}} 

document {
  Key => {(latticePoints,ToricDivisor)},
  Headline => "computes the lattice points in the associated polytope",
  Usage => "latticePoints D",
  Inputs => {"D" => ToricDivisor},
  Outputs => {Matrix => " whose columns are the lattice points in the 
              associated polytope"},
  "On a complete normal toric variety, the polyhedron associated to a Cartier
  divisor is a lattice polytope.  Given a torus-invariant Cartier divisor on a
  normal toric variety, this method returns an integer matrix whose columns
  correspond to the lattices points contained in the associated polytope.  
  For a non-effective Cartier divisor, this method returns ", TO null, ".",
  PARA {},
  "On the projective plane, the associate polytope is either empty, a point,
   or a triangle.",  
  EXAMPLE lines ///
    PP2 = projectiveSpace 2;
    vertices (-PP2_0)
    null === vertices (- PP2_0)
    latticePoints (0*PP2_0)
    isAmple PP2_0
    V1 = latticePoints (PP2_0)
    X1 = normalToricVariety V1;
    set rays X1 === set rays PP2
    max X1 === max PP2
    isAmple (2*PP2_0)
    V2 = latticePoints (2*PP2_0)
    X2 = normalToricVariety(V2, MinimalGenerators => true);
    rays X2 === rays X1
    max X2 === max X1    
    ///,    
  "In this singular example, we see that all the lattice points in the 
  polytope arising from a divisor ", TEX ///$2D$///, " do not come from the 
  lattice points in the polytope arising from ", TEX ///$D$///, ".",
  EXAMPLE lines ///
    Y = normalToricVariety matrix {{0,1,0,0,1},{0,0,1,0,1},{0,0,0,1,1},{0,0,0,0,3}};
    D = 3*Y_0;
    latticePoints D
    latticePoints (2*D)
    ///,        
  SeeAlso => { 
    "Working with divisors and their associated groups",
    (normalToricVariety,Matrix),
    (vertices,ToricDivisor)}} 

document {
  Key => {(polytope,ToricDivisor)},
  Headline => "makes the associated 'Polyhedra' polyhedron",
  Usage => "polytope D",
  Inputs => {"D" => ToricDivisor},
  Outputs => {Polyhedron},
  
  "For a torus-invariant Weil divisors ", TEX ///$D = \sum_i a_i D_i$///, 
  " the associated polyhedron is ", 
  TEX ///$\{ m \in M : (m, v_i) \geq -a_i \forall i \}$///,
  ".  Given a torus-invariant Weil divisor, this methods makes the associated
  polyhedra as an object in ", TO Polyhedra, ".",
  EXAMPLE lines ///
    PP2 = projectiveSpace 2;
    polytope (-PP2_0)
    polytope (0*PP2_0)
    P = polytope (PP2_0)
    vertices P
    ///,    
  "This method works with ", TEX ///$\QQ$///, "-Cartier divisors.",
  EXAMPLE lines ///
    Y = normalToricVariety matrix {{0,1,0,0,1},{0,0,1,0,1},{0,0,0,1,1},{0,0,0,0,3}};
    isCartier Y_0
    isQQCartier Y_0
    polytope Y_0
    vertices polytope Y_0
    ///,        
  "It also works divisors on non-complete toric varieties.",
  EXAMPLE lines ///
    Z = normalToricVariety({{1,0},{1,1},{0,1}},{{0,1},{1,2}});
    isComplete Z
    D = - toricDivisor Z
    P = polytope D
    rays P
    vertices P
    ///,
  SeeAlso => { 
    "Working with divisors and their associated groups",
    (normalToricVariety,Matrix),
    (vertices,ToricDivisor)}} 

undocumented{ nef, (nef,NormalToricVariety) }
   

   

--------------------------------------------------------------------------------
-- total coordinate rings and coherent sheaves

document { 
  Key => "Total coordinate rings and coherent sheaves",
  HREF("http://www3.amherst.edu/~dacox/", "David A. Cox"), " introduced the
  total coordinate ring ", TEX ///$S$///, " of a normal toric variety ", 
  TEX ///$X$///, " and the irrelevant ideal ", TEX ///$B$///, ".  The 
  polynomial ring ", TEX ///$S$///, " has one variable for each ray in the 
  associated fan and a natural grading by the class group.  The monomial 
  ideal ", TEX ///$B$///, " encodes the maximal cones.  The following results 
  of Cox indicate the significance of the pair ", TEX ///$(S,B)$///, ".",
  UL {
    {"The variety ", TEX ///$X$///, " is a good categorial quotient of ", 
    TEX ///${\rm Spec}(S) - {\rm V}(B)$///, " by a suitable group action."},
    {"The category of coherent sheaves on ", TEX ///$X$///, " is equivalent to
    the quotient of the category of finitely generated graded ", 
    TEX ///$S$///, "-modules by the full subcategory of ", TEX ///$B$///, 
    "-torsion modules."}},
  "In particular, we may represent any coherent sheaf on ", TEX ///$X$///, 
  " by giving a finitely generated graded ", TEX ///$S$///, "-module.",
  PARA{},
  "The following methods allow one to make and manipulate coherent sheaves on
  normal toric varieties.",
  SUBSECTION "Menu",
  UL {
    TO (ring,NormalToricVariety),
    TO (ideal,NormalToricVariety),
    TO (sheaf,NormalToricVariety,Ring),	  	  
    TO (sheaf,NormalToricVariety,Module),
    TO (symbol SPACE, OO, ToricDivisor),
    TO (cotangentSheaf, NormalToricVariety),	  
    TO (cohomology,ZZ,NormalToricVariety,CoherentSheaf)},
  SeeAlso =>{
    "Making normal toric varieties",
    "Basic invariants and properties of normal toric varieties",
    "Total coordinate rings and coherent sheaves",
    "Resolution of singularities"}}

document { 
  Key => {(ring, NormalToricVariety)},
  Headline => "make the total coordinate ring (a.k.a. Cox ring)",
  Usage => "ring X",
  Inputs => {"X" => NormalToricVariety},
  Outputs => {PolynomialRing => "the total coordinate ring"},
  "The total coordinate ring (a.k.a. the Cox ring) of a normal toric variety 
  is a polynomial ring in which the variables correspond to the rays in the 
  fan.  The map from the group of torus-invarient Weil divisors to the class 
  group endows this ring with a grading by the ", TO2(cl,"class group"), ".",
  PARA{},
  "The total coordinate ring for projective space is the standard graded
  polynomial ring.",
  EXAMPLE lines ///
    PP3 = projectiveSpace 3;
    S = ring PP3;
    isPolynomialRing S
    gens S
    degrees S
    numgens S == #rays PP3
    coefficientRing S
    ///,     
  "For a product of projective spaces, the total coordinate ring has a
  bigrading.",
  EXAMPLE lines ///
    X = projectiveSpace(2) ** projectiveSpace(3);
    gens ring X
    degrees ring X
    ///,
  "A Hirzebruch surface also has a ", TEX ///$\ZZ^2$///, "-grading.",
  EXAMPLE lines ///
    FF3 = hirzebruchSurface 3;
    gens ring FF3
    degrees ring FF3
    ///,
  Caveat => "The total coordinate ring is not yet implemented when the toric
            variety is degenerate or the class group has torsion.",
  SeeAlso => {
    "Total coordinate rings and coherent sheaves",
    (rays,NormalToricVariety), 
    cl, 
    WeilToClass,
    (fromWDivToCl,NormalToricVariety),
    (ideal, NormalToricVariety), 
    (sheaf,NormalToricVariety,Module)}}

document { 
  Key => {(normalToricVariety, Ring),
    (variety,Ring)},
  Headline => "get the associated normal toric variety",
  Usage => "normalToricVariety S",
  Inputs => {"S" => Ring},
  Outputs => {NormalToricVariety},
  "If a polynomial ring is made as the total coordinate ring of normal toric
  variety, then this method returns the associated variety.",
  EXAMPLE lines ///
    PP3 = projectiveSpace 3;
    S = ring PP3
    gens S
    degrees S
    normalToricVariety S
    variety S
    ///,     
  "If the polynomial ring is not constructed from a variety, then this method
  returns ", TO null, ".",
  EXAMPLE lines ///
    S = QQ[x_0..x_2];
    gens S
    degrees S
    variety S    
    null === variety S
    ///,
  SeeAlso => {
    "Total coordinate rings and coherent sheaves",
    (ring, NormalToricVariety)}}

document { 
  Key => {(ideal, NormalToricVariety), 
    (monomialIdeal, NormalToricVariety)},
  Headline => "make the irrelevant ideal",
  Usage => "ideal X",
  Inputs => {"X" => NormalToricVariety},
  Outputs => {Ideal => {"in the total coordinate ring of ", TT "X", "."}},
  "The irrelevant ideal is a reduced monomial ideal in the total coordinate 
  ring which encodes the combinatorics of the fan.  For each maximal cone in 
  the fan, it has a minimal generator, namely the product of the variables 
  not indexed by elements of the list corresponding to the maximal cone.",
  PARA{},
  "For projective space, the irrelevant ideal is generated by the variables.",
  EXAMPLE lines ///
    PP4 = projectiveSpace 4;
    B = ideal PP4
    isMonomialIdeal B
    ///,
  "For an affine toric variety, the irrelevant ideal is the unit ideal.",
  EXAMPLE lines ///
    C = normalToricVariety({{1,0,0},{0,1,0},{0,0,1},{1,1,-1}},{{0,1,2,3}});
    ideal C
    ///,	  
  "The irrelevant ideal for a product of toric varieties is intersection of 
  the irrelevant ideal of the factors.",
  EXAMPLE lines ///
    X = projectiveSpace(3) ** projectiveSpace(4);
    S = ring X;
    B = ideal X
    primaryDecomposition B
    dual monomialIdeal B
    ///,
  "For a complete simplicial toric variety, the irrelevant ideal is the
  Alexander dual of the Stanley-Reisner ideal of the fan.",
  EXAMPLE lines ///
    Y = smoothFanoToricVariety(2,3);
    max Y
    dual monomialIdeal Y
    ///,
  "Since the irrelevent ideal is a monomial ideal, the command ", 
  TT "monomialIdeal", " also produces the irrelevant ideal.",
  EXAMPLE lines ///
    code(monomialIdeal, NormalToricVariety)
    ///,
  SeeAlso => {
    "Total coordinate rings and coherent sheaves",    
    (max,NormalToricVariety), 
    (ring,NormalToricVariety)}}     

document { 
  Key => {(sheaf,NormalToricVariety,Module)},
  Headline => "make a coherent sheaf",
  Usage => "sheaf(X,M)",
  Inputs => {
    "X" => NormalToricVariety,
    "M" => {"a graded ", TO "module", " over the total coordinate ring"}},
  Outputs => {CoherentSheaf => {"the coherent sheaf on ", TT "X", "
	                        corresponding to ", TT "M"}},
  "The category of coherent sheaves on a normal toric variety is equivalent to
  the quotient category of finitely generated modules over the total 
  coordinate ring by the full subcategory of torsion modules with respect to 
  the irrelevant ideal.  In particular, each finitely generated module over 
  the total coordinate ring corresponds to coherent sheaf on the normal toric 
  variety and every coherent sheaf arises in this manner.",
  PARA {},
  "Free modules correspond to reflexive sheaves.",
  EXAMPLE lines ///
    PP3 = projectiveSpace 3;
    F = sheaf(PP3, (ring PP3)^{{1},{2},{3}})
    ///,
  EXAMPLE lines ///
    FF7 = hirzebruchSurface 7;
    G = sheaf(FF7, (ring FF7)^{{1,0},{0,1}})
    ///,
  SeeAlso => {
    "Total coordinate rings and coherent sheaves",
    (ring,NormalToricVariety),
    (ideal,NormalToricVariety),
    (sheaf,NormalToricVariety)}}     

document { 
  Key => {(sheaf,NormalToricVariety,Ring), 
    (symbol _,OO,NormalToricVariety),
    (sheaf,NormalToricVariety)},
  Headline => "make a coherent sheaf of rings",
  Usage => "sheaf(X,S)",
  Inputs => {
    "X" => NormalToricVariety,
    "S" => {"the total coordinate ring of ", TT "X"}},
  Outputs => {SheafOfRings => {"the structure sheaf on ", TT "X"}},
  "The category of coherent sheaves on a normal toric variety is equivalent to
  the quotient category of finitely generated modules over the total 
  coordinate ring by the full subcategory of torsion modules with respect to 
  the irrelevant ideal.  In particular, the total coordinate ring corresponds 
  to the structure sheaf.",
  PARA{},
  "On projective space, we can make the structure sheaf in a few ways.",
  EXAMPLE lines ///
    PP3 = projectiveSpace 3;
    F = sheaf(PP3, ring PP3)
    G = sheaf PP3
    F === G
    H = OO_PP3
    F === H
    ///,
  SeeAlso => {
    "Total coordinate rings and coherent sheaves",        
    (ring,NormalToricVariety),
    (sheaf,NormalToricVariety,Module)}}    

document { 
  Key => {(cotangentSheaf,NormalToricVariety)},
  Headline => "make the sheaf of Zariski 1-forms",
  Usage => "cotangentSheaf X",
  Inputs => {
    "X" => NormalToricVariety,
    Minimize => Boolean => {" whether to apply ", TO minimalPresentation, " to
                            the result before returning it"}},
  Outputs => {CoherentSheaf => {" the sheaf of Zariski 1-forms on ", TT "X"}},
  "For a normal variety, the sheaf of Zariski 1-forms is defined to be the
  double dual of the cotangent bundle or equivalently the extension of the 
  sheaf of 1-forms on the smooth locus to the entire variety (the complement 
  of the smooth locus has codimension at least two because the variety is 
  normal).  By construction, this sheaf is reflexive with rank equal to the 
  dimension of the variety.  When the underlying variety is smooth, this is 
  simple the sheaf of 1-forms or the cotangent bundle.",
  PARA{},
  "On a non-degenerate normal toric variety, the sheaf of Zariski 1-forms is
  associated to the kernel of a map from the character lattice tensor the 
  total coordinate ring to the direct sum over the rays of the quotient of 
  the total coordinate ring by the ideal generated by the corresponding 
  variable.",
  EXAMPLE lines ///
    PP3 = projectiveSpace 3;
    OmegaPP3 = cotangentSheaf PP3
    prune exteriorPower(3,OmegaPP3)
    prune exteriorPower(3,OmegaPP3) === OO toricDivisor PP3
    ///,
  EXAMPLE lines ///
    X = hirzebruchSurface 2;
    OmegaX = cotangentSheaf X
    prune exteriorPower(dim X, OmegaX)
    prune exteriorPower(dim X, OmegaX) === OO toricDivisor X
    ///,
  EXAMPLE lines ///    
    Rho = {{1,0,0},{0,1,0},{0,0,1},{0,-1,-1},{-1,0,-1},{-2,-1,0}};
    Sigma = {{0,1,2},{0,1,3},{1,3,4},{1,2,4},{2,4,5},{0,2,5},{0,3,5},{3,4,5}};
    Y = normalToricVariety(Rho,Sigma);
    isSmooth Y
    isProjective Y
    OmegaY = cotangentSheaf Y
    prune exteriorPower(dim Y, OmegaY)
    prune exteriorPower(dim Y, OmegaY) === OO toricDivisor Y    
    ///,         
  SeeAlso => {
    "Total coordinate rings and coherent sheaves",        
    (sheaf,NormalToricVariety,Module)}}    

undocumented{ (cotangentSheaf,ZZ,NormalToricVariety) }

document { 
  Key => {(cohomology,ZZ,NormalToricVariety,CoherentSheaf),
    (cohomology,ZZ,NormalToricVariety,SheafOfRings)},
  Headline => "compute the cohomology of a coherent sheaf",
  Usage => "HH^i(X,F)",
  Inputs => {"i" => ZZ,
    "X" => NormalToricVariety,
    "F" => CoherentSheaf => {" on ", TT "X"}},
  Outputs => {{"the ", TT "i", "-th cohomology group of ", TT "F"}},
  "The cohomology functor ", TT "HH", TT SUP "i", TT "(X,-)", " from the
  category of sheaves of abelian groups to the category of abelian groups is 
  the right derived functor of the global sections functor.",
  PARA {},
  "As a simple example, we compute the dimensions of the cohomology groups for
  some line bundles on the projective plane.",
  EXAMPLE {
    "PP2 = projectiveSpace 2;",
    "HH^0(PP2,OO_PP2(1))",
    "apply(10, i -> HH^2(PP2,OO_PP2(-i)))",
    ///loadPackage "BoijSoederberg";///,
    ///loadPackage "BGG";///,
    "cohomologyTable(CoherentSheaf,NormalToricVariety,ZZ,ZZ):=CohomologyTally=>(
      (F,X,lo,hi) -> new CohomologyTally from select(flatten apply(1+dim X, 
          j -> apply(toList(lo-j..hi), i -> {(j,i),rank HH^j(X,F(i))})), 
        p -> p#1 != 0));",
    "cohomologyTable(OO_PP2^1,PP2,-10,10)",
    },
  "Compare this table with the first example in ", TO "BGG::cohomologyTable",
  ".",
  PARA{},
  "For a second example, we compute the dimensions of the cohomology groups 
  for some line bundles on a Hirzebruch surface",
  EXAMPLE {
    "cohomologyTable(ZZ,CoherentSheaf,List,List):=(k,F,lo,hi)->(
      new CohomologyTally from select(flatten apply(toList(lo#0..hi#0),
	  j -> apply(toList(lo#1..hi#1), 
	    i -> {(j,i-j), rank HH^k(variety F, F(i,j))})), 
	p -> p#1 != 0));",
    "FF2 = hirzebruchSurface 2;",
    "cohomologyTable(0,OO_FF2^1,{-7,-7},{7,7})",
    "cohomologyTable(1,OO_FF2^1,{-7,-7},{7,7})",
    "cohomologyTable(2,OO_FF2^1,{-7,-7},{7,7})",
    },
  PARA{},     
  "When ", TT "F", " is free, the algorithm based on Diane Maclagan, Gregory
  G. Smith, ", HREF("http://arxiv.org/abs/math.AC/0305214", "Multigraded
  Castelnuovo-Mumford regularity"), ", ", EM "J. Reine Angew. Math. ", 
  BOLD "571", " (2004), 179-212.  The general case uses the methods described in
  David Eisenbud, Mircea Mustata, Mike Stillman, ",
  HREF("http://arxiv.org/abs/math.AG/0001159", "Cohomology on toric varieties
  and local cohomology with monomial supports"), ", ", EM "J. Symbolic
  Comput. ", BOLD "29", " (2000), 583-600.",
  SeeAlso => {
    "Total coordinate rings and coherent sheaves",            
    (sheaf,NormalToricVariety,Module),
    (sheaf,NormalToricVariety,Ring)}} 





--------------------------------------------------------------------------------
-- Resolution of singularities    

document { 
  Key => "Resolution of singularities",
  "A variety ", TEX ///$X$///, " has a resolution of singularities if one can

  find a nonsingular variety ", TEX ///$Y$///, " and a proper birational map
  from ", TEX ///$Y$///, " to ", TEX ///$X$///, ".  Every normal toric variety
  has a resolution of singularities given by another normal toric variety.",
  PARA{},
  "The following methods related to resolutions of singularities are currently
  available in this package.",
  SUBSECTION "Menu",
  UL {
    TO (makeSmooth,NormalToricVariety),
    TO (makeSimplicial,NormalToricVariety),
    TO (blowup,List,NormalToricVariety)},
  SeeAlso =>{
    "Making normal toric varieties",
    "Basic invariants and properties of normal toric varieties",
    "Working with divisors and their associated groups",
    "Total coordinate rings and coherent sheaves"}}    

document { 
  Key => {makeSimplicial, 
    (makeSimplicial,NormalToricVariety), 
    [makeSimplicial,Strategy]},
  Headline => "make a simplicial toric variety ",
  Usage => "makeSimplical X",
  Inputs => {
    "X" => NormalToricVariety,
    Strategy => {"either ", TT "0", " or ", TT "1"}},
  Outputs => {NormalToricVariety => " which is simplicial"},
  "A normal toric variety is simplical if every cone in its fan is simplicial
  and a cone is simplicial if its minimal generators are linearly independent
  over ", TEX ///$\QQ$///, ".  In fact, the following conditions on a normal
  toric variety ", TEX ///$X$///, " are equivalent:",
  UL{
    {TEX ///$X$///, " is simplicial;"},
    {"every Weil divisor on ", TEX ///$X$///, " has a positive integer multiple
     that is Cartier;"},
    {TEX ///$X$///, " is ", TEX ///$\QQ$///, "-Cartier;"},
    {"the Picard group of ", TEX ///$X$///, " has finite index in the class
     group of ", TEX ///$X$///, ";"},
    {TEX ///$X$///, " has only finite quotient singularities."}},
  PARA{},
  "Given a normal toric variety, this method makes a simplicial toric variety
  with the same rays by triangulating the non-simplicial maximal cones.  For 
  the ", TT "0", " strategy, the triangulation is constructed by repeated 
  regular subdivisions using random integral weight vectors.  For the ",
  TT "1", " strategy, the triangulation is constructed by repeated pushing
  subdivisions (i.e. blowups at a given ray).",
  EXAMPLE lines ///
    X = normalToricVariety(id_(ZZ^3) | - id_(ZZ^3));
    isSimplicial X
    Y1 = makeSimplicial X;
    isSimplicial Y1
    rays Y1 === rays X
    max Y1
    max X
    Y2 = makeSimplicial(X, Strategy => 1);
    isSimplicial Y2
    rays Y2 === rays X
    max Y2
    ///,    
  "If the initial toric variety is simplicial, then this method simply returns
  it.",
  EXAMPLE lines ///
    PP3 = projectiveSpace 3;
    isSimplicial PP3
    Z = makeSimplicial PP3;
    rays Z === rays PP3
    max Z === max PP3
    ///,
  SeeAlso => {
    "Resolution of singularities",
    (isSimplicial,NormalToricVariety)}}

document { 
  Key => {blowup, 
    (blowup,List,NormalToricVariety),
    (blowup,List,NormalToricVariety,List)},
  Headline => "makes the blowup of a normal toric variety along a torus orbit
              closure",
  Usage => "blowup(s,X,w)",
  Inputs => {
    "s" => List => " of integers indexing a proper torus orbit",
    "X" => NormalToricVariety,
    "v" => List => {" of integers giving a vector in the relative interior of
                    the cone corresponding to ", TT "s", " (optional)"}},
  Outputs => {NormalToricVariety => {" obtained by blowing up ", TT "X", 
                                     " along the torus orbit indexed by ", 
				      TT "s"}},
  "Roughly speaking, the blowup replaces a subspace of a given space with all
  the directions pointing out of that subspace.  The metaphor is inflation of 
  a balloon rather than an explosion.  A blowup is the universal way to turn a
  subvariety into a Cartier divisor.",  
  PARA{},
  "The blowup of a normal toric variety along a torus orbit closure is also a
  normal toric variety.  The fan associated to the blowup is star subdivision 
  or stellar subdivision of the fan of the original toric variety.  More
  precisely, we throw out the star of the cone corresponding to ", TT "s", 
  " and join a vector ", TT "v", " lying the relative interior to the 
  boundary of the star.  When the vector ", TT "v", " is not specified, the 
  ray corresponding to the sum of all rays in the cone corresponding to ", 
  TT "s", " is used.",
  PARA{},
  "The simplest example is blowup of the origin in the affine plane.  Note 
  that the new ray has the largest index.",
  EXAMPLE lines ///
    AA2 = affineSpace 2;
    rays AA2
    max AA2
    Bl0 = blowup({0,1},AA2);
    rays Bl0
    max Bl0
    ///,
  "Here are a few different blowups of a non-simplicial affine toric variety",
  EXAMPLE lines ///
    C = normalToricVariety({{1,0,0},{1,1,0},{1,0,1},{1,1,1}},{{0,1,2,3}});
    isSimplicial C
    Bl1 = blowup({0,1,2,3},C);
    rays Bl1
    max Bl1
    Bl2 = blowup({0,1},C);
    rays Bl2
    max Bl2
    Bl3 = blowup({0,1,2,3},C,{5,3,4});
    rays Bl3
    max Bl3
    Bl4 = blowup({0},C);
    isSimplicial Bl4
    rays Bl4
    max Bl4
    ///,  
  "The third collection of examples illustrate some blowups of a 
  non-simplicial projective toric variety.",
  EXAMPLE lines ///
    X = normalToricVariety (id_(ZZ^3) | (-id_(ZZ^3)));
    rays X
    max X
    isSimplicial X
    isProjective X
    orbits(X,1)
    Bl5 = blowup({0,2},X);
    Bl6 = blowup({6,7},Bl5);
    Bl7 = blowup({1,5},Bl6);
    rays Bl7
    max Bl7
    isSimplicial Bl7
    isProjective Bl7
    Bl8 = blowup({0},X);
    Bl9 = blowup({7},Bl8);
    rays Bl9 === rays X
    isSimplicial Bl9
    isProjective Bl9
    ///,  
  Caveat => {"The method assumes that the list ", TT "v", " corresponds to a
  primitive vector.  In other words, the greatest common divisor of its 
  entries is one.  The method also assumes that ", TT "v", " lies in the 
  relative interior of the cone corresponding to ", TT "s", ".  If either of 
  these conditions fail, then the output will not necessarily be a 
  well-defined normal toric variety."},
  SeeAlso => {
    "Resolution of singularities",
    (orbits,NormalToricVariety),
    (isWellDefined, NormalToricVariety),
    (makeSmooth, NormalToricVariety)}}

document { 
  Key => {makeSmooth, 
    (makeSmooth,NormalToricVariety),
    [makeSmooth,Strategy]},
  Headline => "make a birational smooth toric variety ",
  Usage => "makeSmooth X",
  Inputs => {
    "X" => NormalToricVariety,
    Strategy => {"either ", TT "0", " or ", TT "1"}
    },
  Outputs => {NormalToricVariety => " which is smooth"},
  "Every normal toric variety has a resolution of singularities given by 
  another normal toric variety.  Given a normal toric variety ", 
  TEX ///$X$///, ", this method makes a new smooth toric variety ", 
  TEX ///$Y$///, " which has a proper birational map to ", TEX ///$X$///, 
  ".  The normal toric variety ", TEX ///$Y$///, " is obtained from ", 
  TEX ///$X$///, " by repeatedly blowing up appropriate torus orbit closures 
  (if necessary the ", TO makeSimplicial, " method is also used with the 
  specified strategy).  A minimal number of blow-ups are used.",
  PARA{},  
  "As a simple example, we can resolve a simplicial affine singularity.",
  EXAMPLE lines ///
    U = normalToricVariety({{4,-1},{0,1}},{{0,1}});
    isSmooth U
    V = makeSmooth U;
    isSmooth V
    rays V, max V
    set rays V - set rays U
    ///,
  "There is one additional rays, so only one blowup was needed.",
  PARA{},
  "To resolve the singularities of this simplicial projective fourfold, we
  need eleven blowups.",
  EXAMPLE lines ///
    W = weightedProjectiveSpace {1,2,3,4,5};
    dim W
    isSimplicial W
    isSmooth W
    W' = makeSmooth W;
    isSmooth W'
    R = set rays W' - set rays W
    #R
    ///,  
  "If the initial toric variety is smooth, then this method simply returns 
  it.",
  EXAMPLE lines ///
    AA1 = affineSpace 1;
    AA1 === makeSmooth AA1    
    PP2 = projectiveSpace 2;
    PP2 === makeSmooth PP2
    ///,
  "In the next example, we resolve the singularities of a non-simplicial
  projective threefold.",
  EXAMPLE lines ///
    X = normalToricVariety(id_(ZZ^3) | -id_(ZZ^3));
    isSimplicial X
    isSmooth X
    X' = makeSmooth X;
    isSmooth X'
    R = set rays X' - set rays X    
    #R  
    ///,
  "We also demonstrate this method on a complete simplicial non-projective
  threefold.",
  EXAMPLE lines ///
    Rho = {{-1,-1,1},{3,-1,1},{0,0,1},{1,0,1},{0,1,1},{-1,3,1},{0,0,-1}};
    Sigma = {{0,1,3},{0,1,6},{0,2,3},{0,2,5},{0,5,6},{1,3,4},{1,4,5},{1,5,6},{2,3,4},{2,4,5}};
    Z = normalToricVariety(Rho,Sigma);
    isSimplicial Z
    isSmooth Z
    isComplete Z
    isProjective Z
    Z' = makeSmooth Z;
    isSmooth Z'
    R = set rays Z' - set rays Z
    #R
    ///,  
  "We end with a degenerate example.",
  EXAMPLE lines ///
    Y = normalToricVariety({{1,0,0,0},{0,1,0,0},{0,0,1,0},{1,-1,1,0},{1,0,-2,0}},{{0,1,2,3},{0,4}});
    isDegenerate Y
    Y' = makeSmooth Y;
    isSmooth Y'
    ///,
  Caveat => {"A singular normal toric variety almost never has a unique 
              minimal esolution.  This method returns only of one of the many 
	      minimal resolutions."},
  SeeAlso => {
    "Resolution of singularities",
    (isSmooth,NormalToricVariety),
    (makeSimplicial,NormalToricVariety)}}

------------------------------------------------------------------------------
-- TEST
------------------------------------------------------------------------------

-- test 0
TEST ///
X = affineSpace 1;
assert(isWellDefined X == true)
assert(rays X == {{1}})
assert(max X == {{0}})
assert(dim X == 1)
assert(orbits(X,0) == max X)
assert(isDegenerate X == false)
assert(isSmooth X == true)
assert(isComplete X == false)
assert(isFano X == false)
assert(wDiv X == ZZ^1) 
assert(fromWDivToCl X == 0)
assert(cl X == ZZ^0)
assert(cDiv X == ZZ^1)
assert(fromCDivToWDiv X == id_(ZZ^1))
assert(fromCDivToPic X == 0)
assert(pic X == ZZ^0)
assert(fromPicToCl X == 0)
assert(isEffective X_0 == true)
assert(isEffective( - X_0) == false)
assert(isCartier X_0 == true)
assert(isNef X_0 == false)
assert(OO X_0 === OO_X^1)
assert(degrees ring X === {{}})
assert(ideal X == 1)
assert(cotangentSheaf X === OO_X^1)
assert(makeSimplicial X === X)
assert(makeSmooth X === X)
///

-- test 1
TEST ///
X = projectiveSpace 1;
assert(isWellDefined X === true)
assert(set rays X === set({-1},{1}))
assert(max X == sort subsets(2,1))
assert(dim X === 1)
assert(orbits(X,0) == max X)
assert(isDegenerate X === false)
assert(isSmooth X === true)
assert(isProjective X === true)
assert(isFano X === true)
assert(wDiv X == ZZ^2) 
assert(fromWDivToCl X == map(ZZ^1,ZZ^2, i -> 1_ZZ))
assert(cl X == ZZ^1)
assert(cDiv X == ZZ^2)
assert(fromCDivToWDiv X == id_(ZZ^2))
assert(fromCDivToPic X == map(ZZ^1,ZZ^2, i -> 1_ZZ))
assert(pic X == ZZ^1)
assert(fromPicToCl X == id_(ZZ^1))
assert(isEffective X_0 === true)
assert(X_0 + X_1 === toricDivisor({1,1},X))
assert(2*X_0 === X_0 + X_0)
assert(isVeryAmple X_0 === true)
assert(vertices (2*X_0) == matrix {{0,2}})
assert(latticePoints (2*X_0) == matrix {{0,1,2}})
assert(degrees ring X === {{1},{1}})
assert(ideal X == ideal gens ring X)
assert(cotangentSheaf X === OO_X(-2))
assert(all(5, i -> rank HH^0(X,OO_X(i)) == binomial(1+i,i)))
assert(makeSimplicial X === X)
assert(makeSmooth X === X)
///

-- test 2
TEST ///
n = 4;
X = projectiveSpace n;
assert(isWellDefined X === true)
assert(rays X === {toList(n:-1)} | entries id_(ZZ^n))
assert(max X === subsets(n+1,n))
assert(dim X === n)
assert(orbits(X,1) === sort subsets(n+1,n-1))
assert(orbits(X,2) === sort subsets(n+1,n-2))
assert(isDegenerate X === false)
assert(isSmooth X === true)
assert(isProjective X === true)
assert(isFano X === true)
assert(wDiv X == ZZ^(n+1)) 
assert(fromWDivToCl X == map(ZZ^1,ZZ^(n+1), i -> 1_ZZ))
assert(cl X == ZZ^1)
assert(cDiv X == ZZ^(n+1))
assert(fromCDivToWDiv X == id_(ZZ^(n+1)))
assert(fromCDivToPic X == map(ZZ^1,ZZ^(n+1), i -> 1_ZZ))
assert(pic X == ZZ^1)
assert(fromPicToCl X == id_(ZZ^1))
assert(isEffective X_0 === true)
assert(X_0 + X_1 === toricDivisor({1,1} | toList(n-1:0),X))
assert(2*X_0 === X_0 + X_0)
assert(isVeryAmple X_0 === true)
assert(vertices (2*X_0) == map(ZZ^n,ZZ^1,i -> 0) | 2*id_(ZZ^n))
assert(degrees ring X === toList(n+1 : {1}))
assert(ideal X == ideal gens ring X)
assert(cotangentSheaf(X, Minimize => true) === prune sheaf(X, 
    homology(vars ring X,jacobian ring X)))
assert(all(5, i -> rank HH^0(X,OO_X(i)) == binomial(n+i,i)))
assert(all(5, i -> HH^1(X,OO_X(i)) == 0))
assert(all(5, i -> rank HH^n(X,OO_X(-i-n-1)) == binomial(n+i,i)))
assert(makeSimplicial X === X)
assert(makeSmooth X === X)
///

-- test 3
TEST ///
X = hirzebruchSurface 2;
assert(rays X == {{1,0},{0,1},{-1,2},{0,-1}})
assert(max X == {{0,1},{0,3},{1,2},{2,3}})	  
assert(dim X == 2)	  
assert(orbits(X,0) === max X)
assert(orbits(X,1) === apply(4, i -> {i}))
assert(isDegenerate X === false)
assert(isSmooth X === true)
assert(isProjective X === true)
assert(isFano X === false)
assert(wDiv X == ZZ^4) 
assert(fromWDivToCl X == map(ZZ^2,ZZ^4, matrix{{1,-2,1,0},{0,1,0,1}}))
assert(cl X == ZZ^2)
assert(cDiv X == ZZ^4)
assert(fromCDivToWDiv X == id_(ZZ^4))
assert(fromCDivToPic X == map(ZZ^2,ZZ^4, matrix{{1,-2,1,0},{0,1,0,1}}))
assert(pic X == ZZ^2)
assert(fromPicToCl X == id_(ZZ^2))
assert(isEffective X_0 === true)
assert(X_0 + X_1 === toricDivisor({1,1,0,0},X))
assert(2*X_0 === X_0 + X_0)
assert(isNef X_0 === true)
assert(isNef X_1 === false)
assert(isVeryAmple (X_2+X_3) === true)
assert(vertices (X_2+X_3) === matrix{{0,1,0,3},{0,0,1,1}})
assert(latticePoints (X_2+X_3) === matrix {{0, 1, 0, 1, 2, 3}, {0, 0, 1, 1, 1, 1}})
assert(degrees ring X === {{1,0},{-2,1},{1,0},{0,1}})
S = ring X;
assert(ideal X == intersect(ideal(S_0,S_2),ideal(S_1,S_3)))
assert(makeSimplicial X === X)
assert(makeSmooth X === X)
///

-- test 4
TEST ///
X = weightedProjectiveSpace {1,2,3};
assert(rays X == {{-2,-3},{1,0},{0,1}})
assert(max X == {{0,1},{0,2},{1,2}})
assert(dim X == 2)	  
assert(orbits(X,0) === max X)
assert(orbits(X,1) === apply(3, i -> {i}))
assert(isDegenerate X === false)
assert(isSimplicial X === true)
assert(isSmooth X === false)
assert(isProjective X === true)
assert(isFano X === true)
assert(wDiv X == ZZ^3) 
assert(fromWDivToCl X == map(ZZ^1,ZZ^3, matrix{{1,2,3}}))
assert(cl X == ZZ^1)
assert(cDiv X == ZZ^3)
assert(fromCDivToPic X == map(ZZ^1,ZZ^3, matrix{{1,0,0}}))
assert(pic X == ZZ^1)
assert(fromPicToCl X == map(ZZ^1,ZZ^1, {{-6}}))
assert(isEffective X_0 === true)
assert(X_0 + X_1 === toricDivisor({1,1,0},X))
assert(2*X_0 === X_0 + X_0)
assert(isNef X_0 === true)
assert(isCartier X_0 === false)
assert(isQQCartier X_0 === true)
assert(isAmple X_1 === false)
assert(isAmple (3*X_1) === true)
assert(isVeryAmple (3*X_1) === true)
assert(vertices (6*X_0) === matrix{{0,3,0},{0,0,2}})
assert(OO (6*X_0) === OO (3*X_1))
assert(degrees ring X === apply(3, i -> {i+1}))
assert(ideal X == ideal gens ring X)
Y = makeSmooth X;
assert(isWellDefined Y === true)
assert(isSmooth Y === true)
assert(set rays Y === set {{-2,-3},{1,0},{0,1},{-1,-2},{-1,-1},{0,-1}})
assert(sort max Y === sort {{0,5},{0,4},{1,2},{1,3},{2,4},{3,5}})
///

-- test 5
TEST ///
X = kleinschmidt(9,{1,2,3});
assert(isWellDefined X === true)
assert(isFano X === true)
assert(isSmooth X === true)
assert(pic X === ZZ^2)
///

-- test 6
TEST ///
assert(all(5, i -> (
    X := smoothFanoToricVariety(2,i);
    isWellDefined X and isSmooth X and isFano X)))
assert(all(18, i -> (
    X := smoothFanoToricVariety(3,i);
    isWellDefined X and isSmooth X and isFano X)))
X = smoothFanoToricVariety(2,4);
assert(HH^1(X,OO_X(-2,1,1,-2)) == QQ^2)
///

-- test 7
TEST ///
-- it is odd that this test runs out of memory on a Mac OS X machine with 4GB RAM when run by the makefile, but succeeds when run manually
-- I'm disabling this test because it still consumes too much memory.  For example, it uses 2.5GB under ubuntu 64.
exit 0
setRandomSeed 123456
for i to 4 do (
  j := random(20);
  X := smoothFanoToricVariety(5,10*i+j);
  assert(isSmooth X and isFano X))
setRandomSeed 123456
for i to 4 do (
  j := random(200);
  X := smoothFanoToricVariety(6,100*i+j);
  << 100*i +j << endl;
  assert(isSmooth X and isFano X))
///

-- test 8
TEST ///
Rho = {{1,0,0},{0,1,0},{0,0,1},{0,-1,-1},{-1,0,-1},{-2,-1,0}};
Sigma = {{0,1,2},{0,1,3},{1,3,4},{1,2,4},{2,4,5},{0,2,5},{0,3,5},{3,4,5}};
X = normalToricVariety(Rho,Sigma);
assert(isWellDefined X === true)
assert(dim X === 3)
assert(orbits(X,0) === max X)
assert(orbits(X,2) === apply(6, i -> {i}))
assert(isDegenerate X === false)
assert(isSimplicial X === true)
assert(isSmooth X === false)
assert(isComplete X === true)
assert(isProjective X === false)
assert(isFano X === false)
assert(wDiv X == ZZ^6)
assert(cl X === ZZ^3)
assert(pic X === ZZ^3)
assert(isNef(0*X_0) === true)
assert(all(6, i -> isAmple(X_i) === false))
Y = makeSmooth X;
assert(isWellDefined Y === true)
assert(isSmooth Y === true)
///

-- test 9
TEST ///
setRandomSeed 1
X = normalToricVariety(id_(ZZ^3) | -id_(ZZ^3));
assert(isWellDefined X === true)
assert(set rays X === set entries matrix{{1,1,1},{-1,1,1},{1,-1,1},{-1,-1,1},
    {1,1,-1},{-1,1,-1},{1,-1,-1},{-1,-1,-1}})
assert(dim X === 3)
assert(orbits(X,0) === max X)
assert(orbits(X,2) === apply(8, i -> {i}))
assert(isDegenerate X === false)
assert(isSimplicial X === false)
assert(isSmooth X === false)
assert(isProjective X === true)
assert(isFano X === true)
assert(wDiv X == ZZ^8) 
assert(cl X == (cokernel matrix{{2}})^2 ++ ZZ^5)
assert(pic X == ZZ^1)
assert(fromWDivToCl X * fromCDivToWDiv X == fromPicToCl X * fromCDivToPic X)
assert(isEffective X_0 === true)
assert(isCartier X_0 === false)
K = toricDivisor X;
assert(isCartier K == true)
assert(isNef K == false)
Y = makeSimplicial X;
assert(isWellDefined Y === true)
Y = makeSimplicial X;
assert(isWellDefined Y === true)
assert(isSimplicial Y === true)
assert(isSmooth Y === false)
Y = makeSimplicial(X, Strategy => 1);
assert(isWellDefined Y === true)
assert(isSimplicial Y === true)
assert(isSmooth Y === false)
Z = makeSmooth X;
assert(isWellDefined Z === true)
assert(isSmooth Z === true)
///

-- test 10
TEST ///
X = normalToricVariety({{1,0,0,0},{0,1,0,0},{0,0,1,0},{1,-1,1,0},{1,0,-2,0}},
  {{0,1,2,3},{0,4}});
assert(isWellDefined X === true)
assert(dim X === 4)
assert(orbits(X,0) === {})
assert(orbits(X,1) === {{0,1,2,3}})
assert(orbits(X,2) === {{0,1},{0,3},{0,4},{1,2},{2,3}})
assert(orbits(X,3) === apply(5, i -> {i}))
assert(isDegenerate X === true)
assert(isSimplicial X === false)
assert(isSmooth X === false)
assert(isComplete X === false)
assert(wDiv X == ZZ^5)
assert(cl X == ZZ^2)
assert(pic X == ZZ^1)
assert(fromWDivToCl X * fromCDivToWDiv X == fromPicToCl X * fromCDivToPic X)
assert(isEffective X_0 === true)
assert(isCartier X_0 === false)
Y = makeSimplicial X;
assert(isWellDefined Y === true)
assert(isSimplicial Y === true)
assert(isSmooth Y === false)
Y = makeSimplicial(X, Strategy => 0);
assert(isWellDefined Y === true)
assert(isSimplicial Y === true)
assert(isSmooth Y === false)
Z = makeSmooth X;
assert(isWellDefined Z === true)
assert(isSmooth Z === true)
///



-- test 11
TEST ///
C = normalToricVariety({{1,0,0},{1,1,0},{1,0,1},{1,1,1}},{{0,1,2,3}});
Bl1 = blowup({0,1,2,3},C);
assert(rays Bl1 === {{1,0,0},{1,1,0},{1,0,1},{1,1,1},{2,1,1}})
assert(max Bl1 === {{0,1,4},{0,2,4},{1,3,4},{2,3,4}})
Bl2 = blowup({0,1},C);
assert(rays Bl2 === {{1,0,0},{1,1,0},{1,0,1},{1,1,1},{2,1,0}})
assert(max Bl2 === {{0,2,4},{1,3,4},{2,3,4}})
Bl3 = blowup({0,1,2,3},C,{5,3,4});
assert(rays Bl3 === {{1,0,0},{1,1,0},{1,0,1},{1,1,1},{5,3,4}})
assert(max Bl3 === {{0,1,4},{0,2,4},{1,3,4},{2,3,4}})
Bl4 = blowup({0},C);
assert(isSimplicial Bl4 === true)
assert(rays Bl4 === {{1,0,0},{1,1,0},{1,0,1},{1,1,1}})
assert(max Bl4 === {{0,1,3},{0,2,3}})
X = normalToricVariety (id_(ZZ^3) | (-id_(ZZ^3)));
Bl5 = blowup({0,2},X);
assert(rays Bl5 === rays X | {{1,0,1}})
assert(isProjective Bl5 === true)
assert(isWellDefined Bl5 === true)
Bl6 = blowup({0},X);
assert(rays Bl6 === rays X)
assert(isProjective Bl6 === true)
Bl7 = blowup({7},Bl6);
assert(rays Bl7 === rays X)
assert(isSimplicial Bl7 === true)
assert(isProjective Bl7 === true)
assert(isWellDefined Bl7 === true)
///

-- test 12
TEST ///
Rho = {{1,0,0,0},{0,1,0,0},{0,0,1,0},{0,0,0,1},{0,1,1,1},{1,0,1,-1},
  {1,-1,0,1},{1,1,-1,0},{0,0,0,-1},{-1,0,-1,1},{0,-1,0,0},{-1,1,0,-1},
  {0,0,-1,0},{-1,-1,1,0}};
Sigma = {{0,5},{0,6},{0,7},{1,4},{1,7},{1,11},{2,4},{2,5},{2,13},{3,4},{3,6},
  {3,9},{5,8},{6,10},{7,12},{8,9},{10,11},{12,13}};
X = normalToricVariety(Rho,Sigma);
assert(isComplete X == false)
///

end     

------------------------------------------------------------------------------
-- SCRATCH SPACE
------------------------------------------------------------------------------

restart
uninstallPackage "NormalToricVarieties"
installPackage "NormalToricVarieties"
check "NormalToricVarieties"


needsPackage "NormalToricVarieties";
debug NormalToricVarieties
needsPackage "FourierMotzkin";



-- interesting test but it is currently to slow to be on the basic list.
-- test 
TEST ///
Rho = {{1,0,0,0,0,0},{0,1,0,0,0,0},{0,0,1,0,0,0},{0,0,0,1,0,0},{0,0,0,0,1,0},
  {0,0,0,0,0,1},{-1,-1,-1,-1,-1,-1},{1,1,1,0,0,0},{1,0,0,1,1,0},
  {0,-1,-1,-1,-1,0},{0,1,0,1,0,1},{0,0,1,0,1,1},{-1,-1,0,0,-1,-1},
  {-1,0,-1,-1,0,-1}};
Sigma = {{0,1,3,6,8,9,10,13},{0,1,3,6,8,12,13},{0,1,3,6,9,10,12},{0,1,3,7,8,10},
  {0,1,3,7,8,12},{0,1,3,7,10,12},{0,1,4,5,8,11},{0,1,4,5,8,13},{0,1,4,5,11,13},
  {0,1,4,7,8,11},{0,1,4,7,8,13},{0,1,4,7,11,13},{0,1,5,7,8,10,11},
  {0,1,5,7,9,11,13},{0,1,5,7,9,12},{0,1,5,7,10,12},{0,1,5,8,9,10,13},
  {0,1,5,9,10,12},{0,1,6,7,9,12},{0,1,6,7,9,13},{0,1,6,7,12,13},{0,1,7,8,12,13},
  {0,2,3,5,7,8,10,11},{0,2,3,5,7,10,12},{0,2,3,5,8,11,12},{0,2,3,7,8,12},
  {0,2,4,7,8,11},{0,2,4,7,8,12,13},{0,2,4,7,11,13},{0,2,4,8,11,12},
  {0,2,4,11,12,13},{0,2,5,7,9,11},{0,2,5,7,9,12},{0,2,5,9,11,12},{0,2,6,7,9,11,13},
  {0,2,6,7,9,12},{0,2,6,7,12,13},{0,2,6,9,11,12},{0,2,6,11,12,13},
  {0,3,5,8,9,10},{0,3,5,8,9,12},{0,3,5,9,10,12},{0,3,6,8,9,12},{0,4,5,8,9,11},
  {0,4,5,8,9,13},{0,4,5,9,11,13},{0,4,6,8,9,11,12},{0,4,6,8,9,13},
  {0,4,6,8,12,13},{0,4,6,9,11,13},{0,4,6,11,12,13},{0,5,8,9,11,12},
  {1,2,3,7,8,10,11},{1,2,3,7,8,12},{1,2,3,7,10,12},{1,2,3,8,11,12},
  {1,2,3,10,11,12},{1,2,4,7,8,11},{1,2,4,7,8,12,13},{1,2,4,7,11,13},
  {1,2,4,8,11,12},{1,2,4,11,12,13},{1,2,5,6,7,9,11,13},{1,2,5,6,7,9,12},
  {1,2,5,6,11,12,13},{1,2,5,7,10,11},{1,2,5,7,10,12},{1,2,5,10,11,12},
  {1,2,6,7,12,13},{1,3,4,8,10,11},{1,3,4,8,10,13},{1,3,4,8,11,12},
  {1,3,4,8,12,13},{1,3,4,10,11,12,13},{1,3,6,10,12,13},{1,4,5,8,10,11},
  {1,4,5,8,10,13},{1,4,5,10,11,13},{1,5,6,9,10,12},{1,5,6,9,10,13},
  {1,5,6,10,12,13},{1,5,10,11,12,13},{2,3,5,10,11,12},{2,5,6,9,11,12},
  {3,4,5,8,10,11},{3,4,5,8,10,13},{3,4,5,8,11,12},{3,4,5,8,12,13},
  {3,4,5,10,11,12,13},{3,5,6,8,9,10,13},{3,5,6,8,9,12},{3,5,6,8,12,13},
  {3,5,6,9,10,12},{3,5,6,10,12,13},{4,5,6,8,9,11,12},{4,5,6,8,9,13},
  {4,5,6,8,12,13},{4,5,6,9,11,13},{4,5,6,11,12,13}};
X = normalToricVariety(Rho,Sigma);
Y = makeSimplicial X;
debugLevel = 2;
assert(isWellDefined Y === true)
assert(isSimplicial Y === true)
assert(isProjective Y === true)
Y = makeSimplicial(X, Strategy => 0);
assert(isWellDefined Y === true)
assert(isSimplicial Y === true)
assert(isProjective Y === true)

///



