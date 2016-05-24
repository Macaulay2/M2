

---------------------------------------------------------------

-- WISHLIST
--  -Symmetry group for polytopes








--   INPUT : 'L',   a list of Cones, Polyhedra, rays given by R, 
--     	    	    and (rays,linSpace) given by '(R,LS)'
posHull List := L -> (
     -- This function checks if the inserted pair is a pair of matrices that gives valid rays and linSpace
     isValidPair := S -> #S == 2 and if S#1 == 0 then instance(S#0,Matrix) else instance(S#1,Matrix) and numRows S#0 == numRows S#1;
     -- Checking for input errors  
     if L == {} then error("List of convex objects must not be empty");
     C := L#0;
     -- The first entry in the list determines the ambient dimension 'n'
     n := 0;
     local R;
     local LS;
     if (not instance(C,Cone)) and (not instance(C,Polyhedron)) and (not instance(C,Sequence)) and (not instance(C,Matrix)) then 
	  error ("The input must be cones, polyhedra, rays, or (rays,linSpace).");
     -- Adding the vertices and rays to 'R,LS', depending on the type of 'C'
     if instance(C,Cone) then (
	  n = C#"ambient dimension";
	  R = rays C;
	  LS = linSpace C)
     else if instance(C,Polyhedron) then (
	  n = C#"ambient dimension";
	  R = makePrimitiveMatrix vertices C | rays C;
	  LS = linSpace C)
     else if instance(C,Sequence) then (
	  -- Checking for input errors
	  if not isValidPair C then error("Rays and lineality space must be given as a sequence of two matrices with the same number of rows");
	  R = chkQQZZ(C#0,"rays");
	  n = numRows R;
	  LS = if C#1 == 0 then map(ZZ^n,ZZ^1,0) else chkQQZZ(C#1,"lineality space"))
     else (
	  R = chkQQZZ(C,"rays");
	  n = numRows R;
	  LS = map(ZZ^n,ZZ^1,0));
     --  Adding the rays and lineality spaces to 'R,LS' for each remaining element in 'L', depending on the type of 'C'
     L = apply(drop(L,1), C1 -> (
	       -- Checking for further input errors
	       if (not instance(C1,Cone)) and (not instance(C1,Polyhedron)) and (not instance(C1,Sequence)) and 
		    (not instance(C1,Matrix)) then 
		    error ("The input must be cones, polyhedra, rays, or (rays,lineality space)");
	       if instance(C1,Cone) then (
		    if ambDim C1 != n then error("All Cones and Polyhedra must be in the same ambient space");
		    (rays C1,linSpace C1))
	       else if instance(C1,Polyhedron) then (
		    if ambDim C1 != n then error("All Cones and Polyhedra must be in the same ambient space");
		    (makePrimitiveMatrix vertices C1 | rays C1,linSpace C1))
	       else if instance(C1,Sequence) then (
		    -- Checking for input errors
		    if not isValidPair C1 then error("(Rays,lineality space) must be given as a sequence of two matrices with the same number of rows");
		    if numRows C1#0 != n then error("(Rays,lineality space) must be of the correct dimension.");
		    if C1#1 != 0 then (chkQQZZ(C1#0,"rays"),chkQQZZ(C1#1,"lineality space"))
		    else (chkQQZZ(C1#0,"rays"),{}))
	       else (
		    -- Checking for input errors
		    if numRows C1 != n then error("Rays must be of the correct dimension.");
		    (chkQQZZ(C1,"rays"),{}))));
     LR := flatten apply(L, l -> l#0);
     if LR != {} then R = R | matrix {LR};
     L = flatten apply(L, l -> l#1);
     if L != {} then LS = LS | matrix {L};
     if LS == 0 then posHull R else posHull(R,LS))



-- PURPOSE : Building the Fan 'F'
--   INPUT : 'L',  a list of cones and fans in the same ambient space
--  OUTPUT : The fan of all Cones in 'L' and all Cones in of the fans in 'L' and all their faces
fan = method(TypicalValue => Fan)
fan List := L -> (
     -- Checking for input errors
     if L == {} then error("List of cones and fans must not be empty");
     if (not instance(L#0,Cone)) and (not instance(L#0,Fan)) then error("Input must be a list of cones and fans");
     -- Starting with the first Cone in the list and extracting its information
     C := L#0;
     L = drop(L,1);
     ad := C#"ambient dimension";
     local F;
     if instance(C,Fan) then F = C
     else (
	  rayList := rays C;
	  -- Collecting the rays
	  rayList = apply(numColumns rayList, i-> rayList_{i});
	  -- Generating the new fan
	  F = new Fan from {
	  "generatingObjects" => set {C},
	  "ambient dimension" => ad,
	  "dimension" => dim C,
	  "number of generating cones" => 1,
	  "rays" => set rayList,
	  "number of rays" => #rayList,
	  "isPure" => true,
	  symbol cache => new CacheTable});
     -- Checking the remaining list for input errors and reducing fans in the list
     -- to their list of generating cones
     L = flatten apply(L, C -> if instance(C,Cone) then C else if instance(C,Fan) then maxCones C else 
	  error ("Input must be a list of cones and fans"));       
     -- Adding the remaining cones of the list with 'addCone'
     scan(L, C -> F = addCone(C,F));
     F);


--   INPUT : 'C',  a Cone
--  OUTPUT : The Fan given by 'C' and all of its faces
fan Cone := C -> fan {C};


-- PURPOSE : Building the PolyhedralComplex 'PC'
--   INPUT : 'L',  a list of polyhedra in the same ambient space
--  OUTPUT : The polyhedral complex of all Polyhedra in 'L' and all their faces
polyhedralComplex = method(TypicalValue => PolyhedralComplex)
polyhedralComplex List := L -> (
     -- Checking for input errors
     if L == {} then error("List of polyhedra must not be empty");
     if (not instance(L#0,Polyhedron)) and (not instance(L#0,PolyhedralComplex)) then error("Input must be a list of polyhedra and polyhedral complexes");
     -- Starting with the first Polyhedron in the list and extracting its information
     P := L#0;
     L = drop(L,1);
     ad := P#"ambient dimension";
     local PC;
     if instance(P,PolyhedralComplex) then PC = P
     else (
	  verticesList := vertices P;
	  -- Collecting the vertices
	  verticesList = apply(numColumns verticesList, i-> verticesList_{i});
	  -- Generating the new fan
	  PC = new PolyhedralComplex from {
	       "generatingObjects" => set {P},
	       "ambient dimension" => ad,
	       "dimension" => dim P,
	       "number of generating polyhedra" => 1,
	       "vertices" => set verticesList,
	       "number of vertices" => #verticesList,
	       "isPure" => true,
	       symbol cache => new CacheTable});
     -- Checking the remaining list for input errors and reducing polyhedral complexes in the list
     -- to their list of generating polyhedra
     L = flatten apply(L, e -> if instance(e,Polyhedron) then e else if instance(e,PolyhedralComplex) then maxPolyhedra e else 
	  error ("Input must be a list of polyhedra and polyhedral complexes"));       
     -- Adding the remaining polyhedra of the list with 'addPolyhedron'
     scan(L, e -> PC = addPolyhedron(e,PC));
     PC);

polyhedralComplex Polyhedron := P -> polyhedralComplex {P}


addPolyhedron = method(TypicalValue => PolyhedralComplex)
addPolyhedron (Polyhedron,PolyhedralComplex) := (P,PC) -> (
     -- Checking for input errors
     if P#"ambient dimension" != PC#"ambient dimension" then error("The polyhedra must lie in the same ambient space.");
     -- Extracting data
     GP := maxPolyhedra PC;
     d := dim P;
     inserted := false;
     -- Polyhedra in the list 'GP' are ordered by decreasing dimension so we start compatibility checks with 
     -- the cones of higher or equal dimension. For this we divide GP into two seperate lists
     GP = partition(Pf -> (dim Pf) >= d,GP);
     GP = {if GP#?true then GP#true else {},if GP#?false then GP#false else {}};
     if all(GP#0, Pf ->  (
	       (a,b) := areCompatible(Pf,P);
	       -- if 'Pf' and 'P' are not compatible then there is an error
	       if not a then error("The polyhedra are not compatible");
	       -- if they are compatible and 'P' is a face of 'Pf' then 'C' does not 
	       -- need to be added to 'GP'
	       b != P)) then (
	  -- otherwise 'Pf' is still a generating Polyhedron and has to be kept and the remaining polyhedra
	  -- have to be checked
	  GP = GP#0 | {P} | select(GP#1, Pf -> (
		    (a,b) := areCompatible(Pf,P);
		    if not a then error("The polyhedra are not compatible");
		    -- if one of the remaining polyhedra is a face of 'P' this Polyhedron can be dropped
		    b != Pf));
	  inserted = true)     
     -- Otherwise 'P' was already a face of one of the original polyhedra and does not need to be added
     else GP = flatten GP;
     -- If 'P' was added to the Polyhedron as a generating polyhedron then the codim 1 faces on the boundary have to changed to check for 
     -- completeness
     verticesList := toList PC#"vertices";
     if inserted then (
	  -- The vertices of 'P' have to be added
	  Vm := vertices P;
	  Vm = apply(numColumns Vm, i -> Vm_{i});
	  verticesList = unique(verticesList|Vm));
     -- Saving the polyhedral complex
     new PolyhedralComplex from {
	       "generatingObjects" => set GP,
	       "ambient dimension" => P#"ambient dimension",
	       "dimension" => dim(GP#0),
	       "number of generating polyhedra" => #GP,
	       "vertices" => set verticesList,
	       "number of vertices" => #verticesList,
	       "isPure" => dim first GP == dim last GP,
	       symbol cache => new CacheTable})
     
     
--   INPUT : '(L,PC)',  where 'L' is a list of Polyhedra in the same ambient space as the PolyhedralComplex 'PC'
--  OUTPUT : The original PolyhedralComplex 'PC' together with polyhedra in the list 'L'
addPolyhedron (List,PolyhedralComplex) := (L,PC) -> (     
    -- Checking for input errors
    if L == {} then error("The list must not be empty");
    if (not instance(L#0,Polyhedron)) and (not instance(L#0,PolyhedralComplex)) then error("The list may only contain polyhedra and polyhedral complexes");
    if #L == 1 then addPolyhedron(L#0,PC) else addPolyhedron(drop(L,1),addPolyhedron(L#0,PC)))


--   INPUT : '(PC1,PC2)',  where 'PC1' is a PolyhedralComplex in the same ambient space as the PolyhedralComplex 'PC2'
--  OUTPUT : The original fan 'PC2' together with cones of the fan 'PC1'
addPolyhedron (PolyhedralComplex,PolyhedralComplex) := (PC1,PC2) -> (
     -- Checking for input errors
     if ambDim PC2 != ambDim PC1 then error("The polyhedral complexes must be in the same ambient space");
     L := maxCones PC1;
     addCone(L,PC2))
     




Cone ? Cone := (C1,C2) -> (
     if C1 == C2 then symbol == else (
	  if ambDim C1 != ambDim C2 then ambDim C1 ? ambDim C2 else (
	       if dim C1 != dim C2 then dim C1 ? dim C2 else (
		    R1 := rays C1;
		    R2 := rays C2;
		    if R1 != R2 then (
			 R1 = apply(numColumns R1, i -> R1_{i});
			 R2 = apply(numColumns R2, i -> R2_{i});
			 (a,b) := (set R1,set R2); 
			 r := (sort matrix {join(select(R1,i->not b#?i),select(R2,i->not a#?i))})_{0};
			 if a#?r then symbol > else symbol <)
		    else (
			 R1 = linSpace C1;
			 R2 = linSpace C2;
			 R1 = apply(numColumns R1, i -> R1_{i});
			 R2 = apply(numColumns R2, i -> R2_{i});
			 (c,d) := (set R1,set R2);
			 l := (sort matrix {join(select(R1,i->not d#?i),select(R2,i->not c#?i))})_{0};
			 if c#?l then symbol > else symbol <)))))




	     






areCompatible(Cone,Cone) := (C1,C2) -> (
     if C1#"ambient dimension" == C2#"ambient dimension" then (
	  I := intersection(C1,C2);
	  (isFace(I,C1) and isFace(I,C2),I))
     else (false,emptyPolyhedron(C1#"ambient dimension")))


areCompatible(Polyhedron,Polyhedron) := (P1,P2) -> (
     if P1#"ambient dimension" == P2#"ambient dimension" then (
	  I := intersection(P1,P2);
	  (isFace(I,P1) and isFace(I,P2),I))
     else (false,emptyPolyhedron(P1#"ambient dimension")))


-- PURPOSE : Compute the dual face lattice
dualFaceLattice = method(TypicalValue => List)

--   INPUT : '(k,P)',  where 'k' is an integer between 0 and dim 'P' where P is a Polyhedron
--  OUTPUT :  a list, where each entry gives a face of 'P' of dim 'k'. Each entry is a list
-- 	      of the positions of the defining halfspaces
dualFaceLattice(ZZ,Cone) := (k,C) -> (
     L := faceBuilderCone(dim C - k,C);
     HS := halfspaces C;
     HS = apply(numRows HS, i -> HS^{i});
     apply(L, l -> positions(HS, hs -> all(toList l, v -> hs*v == 0))))


dualFaceLattice(ZZ,Polyhedron) := (k,P) -> (
     L := faceBuilder(dim P - k,P);
     HS := halfspaces P;
     HS = apply(numRows HS#0, i -> ((HS#0)^{i},(HS#1)^{i}));
     apply(L, l -> (
	       l = (toList l#0,toList l#1);
	       positions(HS, hs -> (all(l#0, v -> (hs#0)*v - hs#1 == 0) and all(l#1, r -> (hs#0)*r == 0))))))

--   INPUT : 'P',  a Polyhedron
--  OUTPUT :  a list, where each entry is dual face lattice of a certain dimension going from 0 to dim 'P'
dualFaceLattice Polyhedron := P -> apply(dim P + 1, k -> dualFaceLattice(dim P - k,P))

dualFaceLattice Cone := C -> apply(dim C + 1, k -> dualFaceLattice(dim C - k,C))

faceLattice = method(TypicalValue => List)
faceLattice(ZZ,Polyhedron) := (k,P) -> (
     L := faceBuilder(k,P);
     V := vertices P;
     R := rays P;
     V = apply(numColumns V, i -> V_{i});
     R = apply(numColumns R, i -> R_{i});
     apply(L, l -> (
	       l = (toList l#0,toList l#1);
	       (sort apply(l#0, e -> position(V, v -> v == e)),sort apply(l#1, e -> position(R, r -> r == e))))))

faceLattice(ZZ,Cone) := (k,C) -> (
     L := faceBuilderCone(k,C);
     R := rays C;
     R = apply(numColumns R, i -> R_{i});
     apply(L, l -> sort apply(toList l, e -> position(R, r -> r == e))))


faceLattice Polyhedron := P -> apply(dim P + 1, k -> faceLattice(dim P - k,P))


faceLattice Cone := C -> apply(dim C + 1, k -> faceLattice(dim C - k,C))


faceOf = method(TypicalValue => PolyhedraHash)
faceOf Polyhedron := (cacheValue symbol faceOf)( P -> P)
	  
     	  


-- PURPOSE : Computing the Cone of the Minkowskisummands of a Polyhedron 'P', the minimal 
--           Minkowskisummands, and minimal decompositions
--   INPUT : 'P',  a polyhedron
--  OUTPUT : '(C,L,M)'  where 'C' is the Cone of the Minkowskisummands, 'L' is a list of 
--                      Polyhedra corresponding to the generators of 'C', and 'M' is a 
--                      matrix where the columns give the minimal decompositions of 'P'.
minkSummandCone = method()
minkSummandCone Polyhedron := P -> (
     -- Subfunction to save the two vertices of a compact edge in a matrix where the vertex with the smaller entries comes first
     -- by comparing the two vertices entry-wise
     normvert := M -> ( 
	  M = toList M; 
	  v := (M#0)-(M#1);
	  normrec := w -> if (entries w)#0#0 > 0 then 0 else if (entries w)#0#0 < 0 then 1 else (w = w^{1..(numRows w)-1}; normrec w);
          i := normrec v;
	  if i == 1 then M = {M#1,M#0};
	  M);
     -- If the polyhedron is 0 or 1 dimensional itself is its only summand
     if dim P == 0 or dim P == 1 then (posHull matrix{{1}}, hashTable {0 => P},matrix{{1}})
     else (
	  -- Extracting the data to compute the 2 dimensional faces and the edges
	  d := P#"ambient dimension";
          dP := dim P;
          (HS,v) := halfspaces P;
          (hyperplanesTmp,w) := hyperplanes P;
	  F := apply(numRows HS, i -> intersection(HS,v,hyperplanesTmp || HS^{i},w || v^{i}));
	  F = apply(F, f -> (
		    V := vertices f;
		    R := rays f;
		    (set apply(numColumns V, i -> V_{i}),set apply(numColumns R, i -> R_{i}))));
	  LS := linSpace P;
	  L := F;
	  i := 1;
	  while i < dP-2 do (
	       L = intersectionWithFacets(L,F);
	       i = i+1);
	  -- Collect the compact edges
	  L1 := select(L, l -> l#1 === set{});
	  -- if the polyhedron is 2 dimensional and not compact then every compact edge with the tailcone is a summand
	  if dim P == 2 and (not isCompact P) then (
	       L1 = intersectionWithFacets(L,F);
	       L1 = select(L, l -> l#1 === set{});
	       if #L1 == 0 or #L1 == 1 then (posHull matrix{{1}},hashTable {0 => P},matrix{{1}})
	       else (
		    TailC := rays P;
		    if linSpace P != 0 then TailC = TailC | linSpace P | -linSpace(P);
		    (posHull map(QQ^(#L1),QQ^(#L1),1),hashTable apply(#L1, i -> i => convexHull((L1#i)#0 | (L1#i)#1,TailC)),matrix toList(#L1:{1_QQ}))))
	  else (
	       -- If the polyhedron is compact and 2 dimensional then there is only one 2 faces
	       if dim P == 2 then L1 = {(set apply(numColumns vertices P, i -> (vertices P)_{i}), set {})};
	       edges := {};
	       edgesTable := edges;
	       condmatrix := map(QQ^0,QQ^0,0);
	       scan(L1, l -> (
			 -- for every 2 face we get a couple of rows in the condition matrix for the edges of this 2 face
			 -- for this the edges if set in a cyclic order must add up to 0. These conditions are added to 
			 -- 'condmatrix' by using the indices in edges
			 ledges := apply(intersectionWithFacets({l},F), e -> normvert e#0);
			 -- adding e to edges if not yet a member
			 newedges := select(ledges, e -> not member(e,edges));
			 -- extending the condmatrix by a column of zeros for the new edge
			 condmatrix = condmatrix | map(target condmatrix,QQ^(#newedges),0);
			 edges = edges | newedges;
			 -- Bring the edges into cyclic order
			 oedges := {(ledges#0,1)};
			 v := ledges#0#1;
			 ledges = drop(ledges,1);
			 nledges := #ledges;
			 oedges = oedges | apply(nledges, i -> (
				   i = position(ledges, e -> e#0 == v or e#1 == v);
				   e := ledges#i;
				   ledges = drop(ledges,{i,i});
				   if e#0 == v then (
					v = e#1;
					(e,1))
				   else (
					v = e#0;
					(e,-1))));
			 M := map(QQ^d,source condmatrix,0);
			 -- for the cyclic order in oedges add the corresponding edgedirections to condmatrix
			 scan(oedges, e -> (
				   ve := (e#0#1 - e#0#0)*(e#1);
				   j := position(edges, edge -> edge == e#0);
				   M = M_{0..j-1} | ve | M_{j+1..(numColumns M)-1}));
			 condmatrix = condmatrix || M));
	       -- if there are no conditions then the polyhedron has no compact 2 faces
	       if condmatrix == map(QQ^0,QQ^0,0) then (
		    -- collect the compact edges
		    LL := select(faces(dim P - 1,P), fLL -> isCompact fLL);
		    -- if there is only none or one compact edge then the only summand is the polyhedron itself
		    if #LL == 0 or #LL == 1 then (posHull matrix{{1}}, hashTable {0 => P},matrix{{1}})
		    -- otherwise we get a summand for each compact edge
		    else (
			 TailCLL := rays P;
			 if linSpace P != 0 then TailCLL = TailCLL | linSpace P | -linSpace(P);
			 (posHull map(QQ^(#LL),QQ^(#LL),1),hashTable apply(#LL, i -> i => convexHull(vertices LL#i,TailCLL)),matrix toList(#LL:{1_QQ}))))
	       -- Otherwise we can compute the Minkowski summand cone
	       else (
		    Id := map(source condmatrix,source condmatrix,1);
		    C := intersection(Id,condmatrix);
		    R := rays C;
		    TC := map(ZZ^(P#"ambient dimension"),ZZ^1,0) | P#"rays" | P#"linealitySpace" | -(P#"linealitySpace");
		    v = (vertices P)_{0};
		    -- computing the actual summands
		    summList := hashTable apply(numColumns R, i -> (
			      remedges := edges;
			      -- recursive function which takes 'L' the already computed vertices of the summandpolyhedron,
			      -- the set of remaining edges, the current vertex of the original polyhedron, the current 
			      -- vertex of the summandpolyhedron, and the ray of the minkSummandCone. It computes the
			      -- edges emanating from the vertex, scales these edges by the corresponding factor in mi, 
			      -- computes the vertices at the end of those edges (for the original and for the 
			      -- summandpolyhedron) and calls itself with each of the new vertices, if there are edges 
			      -- left in the list
			      edgesearch := (v,v0,mi) -> (
				   remedges = partition(e -> member(v,e),remedges);
				   Lnew := {};
				   if remedges#?true then Lnew = apply(remedges#true, e -> (
					     j := position(edges, edge -> edge == e);
					     edir := e#0 + e#1 - 2*v;
					     vnew := v0 + (mi_(j,0))*edir;
					     (v+edir,vnew,vnew != v0)));
				   if remedges#?false then remedges = remedges#false else remedges = {};
				   L := apply(select(Lnew, e -> e#2),e -> e#1);
				   Lnew = apply(Lnew, e -> (e#0,e#1));
				   L = L | apply(Lnew, (u,w) -> if remedges =!= {} then edgesearch(u,w,mi) else {});
				   flatten L);
			      mi := R_{i};
			      v0 := map(QQ^d,QQ^1,0);
			      -- Calling the edgesearch function to get the vertices of the summand
			      L := {v0} | edgesearch(v,v0,mi);
			      L = matrix transpose apply(L, e -> flatten entries e);
			      i => convexHull(L,TC)));
		    -- computing the inclusion minimal decompositions
		     onevec := matrix toList(numRows R: {1_QQ});
		     negId := map(source R,source R,-1);
		     zerovec :=  map(source R,ZZ^1,0);
		     Q := intersection(negId,zerovec,R,onevec);
		     (C,summList,vertices(Q))))))
 

-- PURPOSE : Computes the mixed volume of n polytopes in n-space
--   INPUT : 'L'  a list of n polytopes in n-space
--  OUTPUT : the mixed volume
-- COMMENT : Note that at the moment the input is NOT checked!
mixedVolume = method()
mixedVolume List := L -> (
     n := #L;
     Elist := apply(L, P -> apply(faces(dim P -1,P),vertices));
     liftings := apply(n, i -> map(ZZ^n,ZZ^n,1)||matrix{apply(n, j -> random 25)});
     Qlist := apply(n, i -> affineImage(liftings#i,L#i));
     local Qsum;
     Qsums := apply(n, i -> if i == 0 then Qsum = Qlist#0 else Qsum = Qsum + Qlist#i);
     mV := 0;
     Elist = apply(n, i -> apply(Elist#i, e -> (e,(liftings#i)*e)));
     E1 := Elist#0;
     Elist = drop(Elist,1);
     center := matrix{{1/2},{1/2}};
     edgeTuple := {};
     k := 0;
     selectRecursion := (E1,edgeTuple,Elist,mV,Qsums,Qlist,k) -> (
	  for e1 in E1 do (
	       Elocal := Elist;
	       if Elocal == {} then mV = mV + (volume sum apply(edgeTuple|{e1}, et -> convexHull first et))
	       else (
		    Elocal = for i from 0 to #Elocal-1 list (
			 hyperplanesTmp := halfspaces(Qsums#k + Qlist#(k+i+1));
			 hyperplanesTmp = for j from 0 to numRows(hyperplanesTmp#0)-1 list if (hyperplanesTmp#0)_(j,n) < 0 then ((hyperplanesTmp#0)^{j},(hyperplanesTmp#1)^{j}) else continue;
			 returnE := select(Elocal#i, e -> (
				   p := (sum apply(edgeTuple|{e1}, et -> et#1 * center)) + (e#1 * center);
				   any(hyperplanesTmp, pair -> (pair#0)*p - pair#1 == 0)));
			 --if returnE == {} then break{};
			 returnE);
		    mV = selectRecursion(Elocal#0,edgeTuple|{e1},drop(Elocal,1),mV,Qsums,Qlist,k+1)));
	  mV);
     selectRecursion(E1,edgeTuple,Elist,mV,Qsums,Qlist,k))
 
 
objectiveVector = method()
objectiveVector (Polyhedron,Polyhedron) := (P,Q) -> (
     -- Checking for input errors
     if not isFace(Q,P) then error("The second polyhedron must be a face of the first one");
     (HS,w) := halfspaces P;
     V := vertices Q;
     R := rays Q;
     V = apply(numColumns V, i -> V_{i});
     v := select(toList (0..(numRows HS)-1), i -> all(V, v -> HS^{i} * v - w^{i} == 0) and HS^{i} * R == 0);
     sum apply(v, i -> transpose HS^{i}))



-- PURPOSE : Returning a polytope of which the fan is the normal if the fan is polytopal
--   INPUT : 'F',  a Fan
--  OUTPUT : A Polytope of which 'F' is the normal fan
polytope = method(TypicalValue => Polyhedron)
polytope Fan := F -> (
     if not F.cache.?isPolytopal then isPolytopal F;
     if not F.cache.isPolytopal then error("The fan must be polytopal");
     F.cache.polytope)



-- PURPOSE : Computing the 'n'-skeleton of a fan
--   INPUT : (n,F),  where 'n' is a positive integer and
--                   'F' is a Fan
--  OUTPUT : the Fan consisting of the 'n' dimensional cones in 'F'
skeleton = method(TypicalValue => Fan)
skeleton(ZZ,Fan) := (n,F) -> (
     -- Checking for input errors
     if n < 0 or dim F < n then error("The integer must be between 0 and dim F");
     fan cones(n,F))

skeleton(ZZ,PolyhedralComplex) := (n,PC) -> (
     -- Checking for input errors
     if n < 0 or dim PC < n then error("The integer must be between 0 and dim F");
     GP := polyhedra(n,PC);
     verticesList := unique flatten apply(GP, P -> (Vm := vertices P; apply(numColumns Vm, i -> Vm_{i})));
     new PolyhedralComplex from {
	       "generatingObjects" => set GP,
	       "ambient dimension" => ambDim PC,
	       "dimension" => n,
	       "number of generating polyhedra" => #GP,
	       "vertices" => set verticesList,
	       "number of vertices" => #verticesList,
	       "isPure" => true,
	       symbol cache => new CacheTable});
     
-- PURPOSE : Computing the smallest face of 'P' containing 'p'
--   INPUT : '(p,P)',  where 'p' is a point given as a matrix and
--     	    	       'P' is a polyhedron
--  OUTPUT : The smallest face containing 'p' as a polyhedron
smallestFace = method()
smallestFace(Matrix,Polyhedron) := (p,P) -> (
     -- Checking for input errors
     if numColumns p =!= 1 or numRows p =!= P#"ambient dimension" then error("The point must lie in the same space");
     p = chkZZQQ(p,"point");
     -- Checking if 'P' contains 'p' at all
     if contains(P,convexHull p) then (
	  (M,v) := halfspaces P;
     	  (N,w) := hyperplanes P;
     	  -- Selecting the half-spaces that fullfil equality for p
	  -- and adding them to the hyperplanes
	  v = promote(v,QQ);
	  pos := select(toList(0..(numRows M)-1), i -> (M^{i})*p == v^{i});
	  N = N || M^pos;
	  w = w || lift(v^pos,ZZ);
	  intersection(M,v,N,w))
     else emptyPolyhedron P#"ambient dimension")



--   INPUT : '(p,C)',  where 'p' is point given as a matrix and
--     	    	       'C' is a Cone
--  OUTPUT : The smallest face containing 'p' as a cone
smallestFace(Matrix,Cone) := (p,C) -> (
     -- Checking for input errors
     if numColumns p =!= 1 or numRows p =!= C#"ambient dimension" then error("The point must lie in the same space");
     p = chkZZQQ(p,"point");
     -- Checking if 'C' contains 'p' at all
     if contains(C,posHull p) then (
	  M := halfspaces C;
     	  N := hyperplanes C;
     	  -- Selecting the half-spaces that fullfil equality for p
	  -- and adding them to the hyperplanes
	  pos := select(toList(0..(numRows M)-1), i -> (M^{i})*p == 0);
	  N = N || M^pos;
	  intersection(M,N))
     else emptyPolyhedron C#"ambient dimension")


-- PURPOSE : Computing the tail cone of a given Polyhedron
--   INPUT : 'P',  a Polyhedron
--  OUTPUT : The Cone generated by the rays and the lineality space of 'P'
tailCone = method(TypicalValue => Cone)
tailCone Polyhedron := P -> posHull(P#"rays",P#"linealitySpace")




-- PURPOSE : Computing the closest point of a polyhedron to a given point
--   INPUT : (p,P),  where 'p' is a point given by a one column matrix over ZZ or QQ and
--                   'P' is a Polyhedron
--  OUTPUT : the point in 'P' with the minimal euclidian distance to 'p'
proximum = method(TypicalValue => Matrix)
proximum (Matrix,Polyhedron) := (p,P) -> (
     -- Checking for input errors
     if numColumns p =!= 1 or numRows p =!= P#"ambient dimension" then error("The point must lie in the same space");
     if isEmpty P then error("The polyhedron must not be empty");
     -- Defining local variables
     local Flist;
     d := ambDim P;
     c := 0;
     prox := {};
     -- Checking if 'p' is contained in 'P'
     if contains(P,p) then p
     else (
	  V := vertices P;
	  R := promote(rays P,QQ);
	  -- Distinguish between full dimensional polyhedra and not full dimensional ones
	  if dim P == d then (
	       -- Continue as long as the proximum has not been found
	       while instance(prox,List) do (
		    -- Take the faces of next lower dimension of P
		    c = c+1;
		    if c == dim P then (
			 Vdist := apply(numColumns V, j -> ((transpose(V_{j}-p))*(V_{j}-p))_(0,0));
			 pos := min Vdist;
			 pos = position(Vdist, j -> j == pos);
			 prox = V_{pos})
		    else (
			 Flist = faces(c,P);
			 -- Search through the faces
			 any(Flist, F -> (
				   -- Take the inward pointing normal cone with respect to P
				   (vL,bL) := hyperplanes F;
				   -- Check for each ray if it is pointing inward
				   vL = matrix apply(numRows vL, i -> (
					     v := vL^{i};
					     b := first flatten entries bL^{i};
					     if all(flatten entries (v*(V | R)), e -> e >= b) then flatten entries v
					     else flatten entries(-v)));
				   -- Take the polyhedron spanned by the inward pointing normal cone 
				   -- and 'p' and intersect it with the face
				   Q := intersection(F,convexHull(p,transpose vL));
				   -- If this intersection is not empty, it contains exactly one point, 
				   -- the proximum
				   if not isEmpty Q then (
					prox = vertices Q;
					true)
				   else false))));
	       prox)
	  else (
	       -- For not full dimensional polyhedra the hyperplanes of 'P' have to be considered also
	       while instance(prox,List) do (
		    if c == dim P then (
			 Vdist1 := apply(numColumns V, j -> ((transpose(V_{j}-p))*(V_{j}-p))_(0,0));
			 pos1 := min Vdist1;
			 pos1 = position(Vdist1, j -> j == pos1);
			 prox = V_{pos1})
		    else (
			 Flist = faces(c,P);
			 -- Search through the faces
			 any(Flist, F -> (
				   -- Take the inward pointing normal cone with respect to P
				   (vL,bL) := hyperplanes F;
				   vL = matrix apply(numRows vL, i -> (
					     v := vL^{i};
					     b := first flatten entries bL^{i};
					     entryList := flatten entries (v*(V | R));
					     -- the first two ifs find the vectors not in the hyperspace
					     -- of 'P'
					     if any(entryList, e -> e > b) then flatten entries v
					     else if any(entryList, e -> e < b) then flatten entries(-v)
					     -- If it is an original hyperplane than take the direction from 
					     -- 'p' to the polyhedron
					     else (
						  bCheck := first flatten entries (v*p);
						  if bCheck < b then flatten entries v
						  else flatten entries(-v))));
				   Q := intersection(F,convexHull(p,transpose vL));
				   if not isEmpty Q then (
					prox = vertices Q;
					true)
				   else false)));
		    c = c+1);
	       prox)))


--   INPUT : (p,C),  where 'p' is a point given by a one column matrix over ZZ or QQ and
--                   'C' is a Cone
--  OUTPUT : the point in 'C' with the minimal euclidian distance to 'p'
proximum (Matrix,Cone) := (p,C) -> proximum(p,coneToPolyhedron C)


-- PURPOSE : Computing the affine hull
--   INPUT : 'P',  a Polyhedron
--  OUTPUT : the affine hull of 'P' as a Polyhedron
affineHull = method(TypicalValue => Polyhedron)
affineHull Polyhedron := P -> (
     M := vertices P;
     R := promote(rays P,QQ);
     -- subtracting the first vertex from all other vertices
     N := (M+M_{0}*(matrix {toList(numColumns M:-1)}))_{1..(numColumns M)-1};
     convexHull(M_{0},N | -N | R | -R));



-- PURPOSE : Computes the coarsest common refinement of a given set of rays
--   INPUT : 'M'  a Matrix
--  OUTPUT : 'F'  a Fan, the coarsest common refinement of the rays in 'M'
ccRefinement = method(TypicalValue => Fan)
ccRefinement Matrix := M -> (
     -- Checking for input errors
     M = chkZZQQ(M,"rays");
     -- Extracting data
     n := numRows M;
     m := numColumns M;
     -- Generating all cones generated by 'n' rays in 'M'
     nCones := apply(subsets(m,n), e -> posHull M_e);
     -- Selecting those cones that are 'n' dimensional and do not contain any 
     -- of the others
     nConesfd := select(nCones, C -> dim C == n);
     nConesfd = inclMinCones nConesfd;
     refCones := {};
     while nConesfd != {} do (
	  newCones := {};
	  -- scan through the 'n' dimensional cones and check for each of the cones generated by
	  -- 'n' rays if their intersection is 'n' dimensional and if the first one is not contained 
	  -- in the latter. If true, then their intersection will be saved in the list 'newCones'.
	  -- If false for every cone generated by 'n' rays, then the 'n' dimensional cone will be 
	  -- appended to the list 'refCones'
	  refCones = refCones | (flatten apply(nConesfd, C1 -> (
			 toBeAdded := flatten apply(nCones, C2 -> (
				   C := intersection(C2,C1);
				   if dim C == n and (not contains(C2,C1)) then C
				   else {}));
			 if toBeAdded == {} then C1
			 else (
			      newCones = newCones | toBeAdded;
			      {}))));
	  -- now, the new intersections will be the 'n' dimensional cones and the same procedure 
	  -- starts over again if this list is not empty
	  nConesfd = unique newCones);
     -- Compute the fan generated by the 'refCones'
     fan refCones);


-- PURPOSE : Converts the Cone 'C' into itself as a Polyhedron 'P'
--   INPUT : 'C'  a Cone
--  OUTPUT : 'P' the Cone saved as a polyhedron
coneToPolyhedron = method(TypicalValue => Polyhedron)
coneToPolyhedron Cone := C -> (
     M := map(QQ^(C#"ambient dimension"),QQ^1,0);
     N := rays C;
     convexHull(M,N))




dualCayley = method(TypicalValue => Polyhedron)
dualCayley Polyhedron := P -> (
     V := vertices P;
     (M,N) := fourierMotzkin V;
     M = sort(map(QQ^1,source M,(i,j) -> 1)|| -M);
     R := map(target M,QQ^0,0);
     HS := map(QQ^1,source V,0) || -V;
     (hyperA,verticesA) := fMReplacement(HS,M,R);
     polyhedronBuilder(hyperA,verticesA)) 


dualCayleyFace = method(TypicalValue => Polyhedron)
dualCayleyFace Polyhedron := (cacheValue symbol dualCayleyFace)(P -> (
	  local Pd;
	  --local faceOf;
	  if P.cache.?faceOf then (
	       V := transpose vertices P;
	       R := transpose rays P;
	       P0 := P.cache.faceOf;
	       P0d := dualCayley P0;
	       codimensionPd := dim P - P0#"dimension of lineality space" + 1;
	       L := faces(codimensionPd,P0d);
	       Pd = first select(1,L, l -> (V || R)*(vertices l | rays l) == 0);
	       Pd.cache.dualCayleyFace = P;
	       Pd)
	  else (
	       Pdual := dualCayley P;
	       Pd = first faces(dim P + 1,P);
	       Pd.cache.dualCayleyFace = P;
	       Pd))) 
     

-- PURPOSE : Computing the dual cone
--   INPUT : 'C',  a Cone
--  OUTPUT : The dual Cone, which is {v | v*c>=0 forall c in C}
dualCone = method(TypicalValue => Cone)
dualCone Cone := C -> (
	genrays := (sort transpose C#"halfspaces",sort transpose C#"hyperplanes");
	dualgens := (sort (-(C#"rays")),sort C#"linealitySpace");
	coneBuilder(genrays,dualgens))
   

-- PURPOSE : Computing the face fan of a polytope
--   INPUT : 'P',  a Polyhedron, containing the origin in its interior
--  OUTPUT : The Fan generated by the cones over all facets of the polyhedron
faceFan = method(TypicalValue => Fan)
faceFan Polyhedron := P -> (
     -- Checking for input errors
     if not inInterior(map(QQ^(ambDim P),QQ^1,0),P) then  error("The origin must be an interior point.");
     F := fan apply(faces(1,P), posHull);
     F.cache.isPolytopal = true;
     F.cache.polytope = polar P;
     F)
   
   
-- PURPOSE : Computing the image fan of a cone
--   INPUT : '(M,C)',  a Matrix 'M' and a Cone 'C'
--  OUTPUT : A Fan, the common refinement of the images of all faces of
--     	     'C' under 'M'
imageFan = method(TypicalValue => Fan)
imageFan (Matrix,Cone) := (M,C) -> (
     M = chkZZQQ(M,"map");
     if numColumns M != ambDim C then error("The source space of the matrix must be the ambient space of the cone");
     -- Extracting data
     m := numRows M;
     n := dim C;
     -- Compute the images of all 'm' dimensional faces and select those that are again 
     -- 'm' dimensional
     L := apply(faces(n-m,C), e -> affineImage(M,e));
     L = select(L, e -> dim e == m);
     -- Compute their common refinement
     refineCones L)





-- PURPOSE : Scaling respectively the multiple Minkowski sum of a polyhedron
--   INPUT : '(k,P)',  where 'k' is a strictly positive rational or integer number and 
--     	    	             'P' is a Polyhedron
--  OUTPUT : The polyehdron 'P' scaled by 'k'
QQ * Polyhedron := (k,P) -> (
     -- Checking for input errors
     if k <= 0 then error("The factor must be strictly positiv");
     convexHull(k*(vertices P),rays P | linSpace P))

ZZ * Polyhedron := (k,P) -> promote(k,QQ) * P


-- PURPOSE : Computing the normal cone of a face of a polytope
--   INPUT : '(P,Q)',  two polyhedra
--  OUTPUT : 'C',  a Cone, the inner normal cone of P in the face Q
-- COMMENT : 'Q' must be a face of P
normalCone (Polyhedron,Polyhedron) := Cone => opts -> (P,Q) -> (
     if not P.cache.?normalCone then P.cache.normalCone = new MutableHashTable;
     if not P.cache.normalCone#?Q then (
	  -- Checking for input errors
	  if not isFace(Q,P) then error("The second polyhedron must be a face of the first one");
	  p := interiorPoint Q;
	  P.cache.normalCone#Q = dualCone posHull affineImage(P,-p));
     P.cache.normalCone#Q)


-- PURPOSE : Computing the inner normalFan of a polyhedron
--   INPUT : 'P',  a Polyhedron
--  OUTPUT : 'F',  a Fan, the inner normalFan of 'P'
normalFan = method(TypicalValue => Fan)
normalFan Polyhedron := P -> (
     if not P.cache.?normalFan then (
	  -- Saving the vertices
	  vm := vertices P;
	  -- For every vertex translate P by -this vertex and take the dual cone of the positive hull of it
	  L := sort apply(numColumns vm, i -> (dualCone posHull affineImage(P,-vm_{i})));
	  HS := transpose (halfspaces P)#0;
	  HS = apply(numColumns HS, i -> -HS_{i});
	  F := new Fan from {
	       "generatingObjects" => set L,
	       "ambient dimension" => ambDim P,
	       "dimension" => dim L#0,
	       "number of generating cones" => #L,
	       "rays" => set HS,
	       "number of rays" => #HS,
	       "isPure" => true,
	       symbol cache => new CacheTable};
	  F.cache.isPolytopal = true;
	  F.cache.polytope = P;
	  P.cache.normalFan = F);
     P.cache.normalFan)


-- PURPOSE : Computing the polar of a given polyhedron
--   INPUT : 'P',  a Polyhedron
--  OUTPUT : A Polyhedron, the set { v | v*p<=1 forall p in P}
polar = method(TypicalValue => Polyhedron)
polar Polyhedron := (cacheValue symbol polar)(P -> (
     d := P#"ambient dimension";
     -- Make the 'd'-dimensional identity
     M := map(ZZ^d,ZZ^d,-1);
     -- make the block matrix of -1 and the 'd'identity
     M = (matrix{{-1_ZZ}} | map(ZZ^1,ZZ^d,0))||(map(ZZ^d,ZZ^1,0) | M);
     hyperA := P#"homogenizedVertices";
     hyperA = (sort (M*(hyperA#0)),hyperA#1);
     verticesA := fourierMotzkin hyperA;
     (hyperA,verticesA) = fMReplacement(hyperA#0,verticesA#0,verticesA#1);
     Q := polyhedronBuilder(hyperA,verticesA);
     Q.cache.polar = P;
     Q))


-- PURPOSE : Compute the corresponding face of the polar polytope
--   INPUT : 'P',  a Polyhedron
--  OUTPUT : A Polyhedron, if 'P' is the face of some polyhedron 'Q' then the
--     	     result is the dual face on the polar of 'Q'. If 'P' is not a face
--           then it is considered as the face of itself and thus the 
--           polarFace is the empty Polyhedron
polarFace = method(TypicalValue => Polyhedron)
polarFace Polyhedron := (cacheValue symbol polarFace)(P -> (
	  local Pd;
	  --local faceOf;
	  if P.cache.?faceOf then (
	       V := transpose vertices P;
	       R := transpose rays P;
	       P0 := P.cache.faceOf;
	       P0d := polar P0;
	       codimensionPd := dim P - P0#"dimension of lineality space" + 1;
	       L := faces(codimensionPd,P0d);
	       Pd = first select(1,L, l -> all(flatten entries(V*(vertices l)),e -> e == -1) and V*(rays l) == 0 and R*(vertices l | rays l) == 0);
	       Pd.cache.polarFace = P;
	       Pd)
	  else (
	       Pdual := polar P;
	       Pd = first faces(dim P + 1,P);
	       Pd.cache.polarFace = P;
	       Pd)))	       
	       

-- PURPOSE : Computing the sublattice basis for a given matrix of lattice points or for the lattice points
--     	     of a given polytope
sublatticeBasis = method(TypicalValue => Matrix)

--   INPUT : 'M',  a Matrix
--  OUTPUT : A matrix, a basis of the sublattice spanned by the lattice points in 'M'
sublatticeBasis Matrix := M -> (
     -- Checking for input errors
     M = chkZZQQ(M,"lattice points");
     M = if promote(substitute(M,ZZ),QQ) == M then substitute(M,ZZ) else error("The matrix must contain only lattice points.");
     -- The sublattice is isomorphic to source mod kernel, i.e. A/K
     A := source M; 
     K := ker M;
     -- Taking minimal generators and applying M gives a basis in target M
     M*(mingens (A/K)))


--   INPUT : 'P',  a polyhedron,
--  OUTPUT : A matrix, a basis of the sublattice spanned by the lattice points of 'P'
sublatticeBasis Polyhedron := P -> (
     L := latticePoints P;
     -- Checking for input errors
     if L == {} then error("The polytope must contain lattice points.");
     -- Translating 'P' so that it contains the origin if it did not already
     if all(L,l -> l != 0) then L = apply(L, l -> l - L#0);
     sublatticeBasis(matrix {L}))
   
   
-- PURPOSE : Calculating the preimage of a polytope in the sublattice generated by its lattice points
--   INPUT : 'P',  a polyhedron
--  OUTPUT : A polyhedron, the projected polyhedron, which is now normal
toSublattice = method()
toSublattice Polyhedron := P -> (
     L := latticePoints P;
     -- Checking for input errors
     if L == {} then error("The polytope must contain lattice points.");
     b := L#0;
     -- Translating 'P' so that it contains the origin if it did not already
     if all(L,l -> l != 0) then L = apply(L, l -> l - L#0);     
     affinePreimage(sublatticeBasis matrix {L},P,b))



-- PURPOSE : Computing the cell decomposition of a compact polyhedron given by a weight vector on the lattice points
--   INPUT : '(P,w)',  where 'P' is a compact polyhedron and 'w' is a one row matrix with with lattice points of 'P' 
--     	    	       many entries
--  OUTPUT : A list of polyhedra that are the corresponding cell decomposition
cellDecompose = method(TypicalValue => List)
cellDecompose (Polyhedron,Matrix) := (P,w) -> (
     n := dim P;
     LP := latticePoints P;
     -- Checking for input errors
     if numColumns w != #LP or numRows w != 1 then error("The weight must be a one row matrix with number of lattice points many entries");
     LP = matrix{LP}||w;
     P = convexHull(LP,matrix (toList(dim P:{0})|{{1}}));
     A := map(QQ^n,QQ^n,1) | map(QQ^n,QQ^1,0);
     flatten apply(faces(1,P), f -> if isCompact f then affineImage(A,f) else {}))


-- PURPOSE : Computing the Ehrhart polynomial of a polytope
--   INPUT : 'P',  a polyhedron which must be compact, i.e. a polytope
--  OUTPUT : A polynomial in QQ[x], the Ehrhart polynomial
-- COMMENT : Compactness is checked within latticePoints
ehrhart = method(TypicalValue => RingElement)
ehrhart Polyhedron := P -> (
	n := dim P;
	v := matrix apply(n,k -> {-1+#latticePoints( (k+1)*P)});
	M := promote(matrix apply(n,i -> reverse apply(n, j -> (i+1)^(j+1))),QQ);
	M = flatten entries ((inverse M)*v);
	R := QQ[getSymbol "x"];
	x := R_"x";
	1+sum apply(n,i -> M#i * x^(n-i)))






-- PURPOSE : Saving the actual Session of Polyhedra (and PPDivisor)
--   INPUT : 'F',  a String, the filename
--  OUTPUT : The file F
--COMMENTS : This function saves not the complete Session, but it saves every convex polyhedral objects assigned to 
--     	     a Symbol, i.e. all Cones, Polyhedra, Fans as well as Matrices and if the additional package 
--     	     "PPDivisor" is loaded it saves also all PolyhedralDivisors. Furthermore all lists and sequences
--     	     that contain any of the types above (to arbitrary depth of lists and sequences) are also saved 
--     	     to the file. But keep in mind that this works only for such objects assigned to a Symbol! The session 
-- 	     can be reovered by calling
--     	     load F
-- 	     It is not neccessary to load Polyhedra before loading the saved session, because if not yet loaded it will
--     	     load Polyhedra. Also if PPDivisor was loaded when the session has been saved it will also be loaded.
saveSession = method()
saveSession String := F -> (
     -- Creating and opening the output file
     F = openOut F;
     -- Make sure Polyhedra is loaded when the session is recovered
     F << "needsPackage \"Polyhedra\"" << endl;
     -- Check if PPDivisor has been loaded
     PPDivisorPackageLoaded :=  PackageDictionary#?"PPDivisor";
     if (PPDivisorPackageLoaded) then (
	  -- if so, make sure it will also be loaded when the session is recovered
	  F << "needsPackage \"PPDivisor\"" << endl);
     --Save all Matrices to the file
     scan(userSymbols Matrix, s -> F << s << " = " << toExternalString value s << endl);
     scan(userSymbols PolyhedraHash, s -> F << s << " = " << toExternalString value s << endl);
     -- Save all Lists and Sequences containing only convex polyhedral objects and/or lists of them to the file
     scan(userSymbols List | userSymbols Sequence, s -> (
	       L := value s;
	       while L =!= flatten L do L = flatten L;
	       if all(L, l -> (
			 if instance(l,Sequence) then all(l, e -> instance(l,PolyhedraHash) or instance(l,Matrix)) 
			 else instance(l,PolyhedraHash) or instance(l,Matrix))) then F << s << " = " << toExternalString value s << endl)))
     



---------------------------------------
-- DECLARING AUXILIARY FUNCTIONS
-- >> not public <<
---------------------------------------

liftable (Matrix,Number) := (f,k) -> try (lift(f,k); true) else false; 

makePrimitiveMatrix = M -> if M != 0 then lift(transpose matrix apply(entries transpose M, w -> (g := abs gcd w; apply(w, e -> e//g))),ZZ) else lift(M,ZZ);
     

fMReplacement = (R,HS,hyperplanesTmp) -> (
     uniqueColumns := M -> matrix{(unique apply(numColumns M, i -> M_{i}))};
     n := numRows R;
     LS := mingens ker transpose(HS|hyperplanesTmp);
     alpha := rank LS;
     if alpha > 0 then (
	  LS = lift(gens gb promote(LS,QQ[]),QQ);
	  CR := mingens ker transpose LS;
	  CR = CR * (inverse(LS|CR))^{alpha..n-1};
	  R = CR * R);
     beta := rank hyperplanesTmp;
     if beta > 0 then (
	  hyperplanesTmp = lift(gens gb promote(hyperplanesTmp,QQ[]),QQ);
	  CHS := mingens ker transpose hyperplanesTmp;
	  CHS = CHS * (inverse(hyperplanesTmp|CHS))^{beta..n-1};
	  HS = CHS * HS);
     HS = if HS == 0 then map(ZZ^(numRows HS),ZZ^0,0) else sort uniqueColumns makePrimitiveMatrix HS;
     R = apply(numColumns R, i -> R_{i});
     R = select(R, r -> (r != 0 and (
		    pos := positions(flatten entries((transpose HS) * r), e -> e == 0);
		    #pos >= n-alpha-beta-1 and (n <= 3 or rank HS_pos >= n-alpha-beta-1))));
     if R == {} then R = map(ZZ^(numRows LS),ZZ^0,0) else R = sort matrix {unique apply(R, makePrimitiveMatrix)};
     LS = if LS == 0 then map(ZZ^(numRows LS),ZZ^0,0) else sort uniqueColumns makePrimitiveMatrix LS;
     hyperplanesTmp = if hyperplanesTmp == 0 then map(ZZ^(numRows hyperplanesTmp),ZZ^0,0) else sort uniqueColumns makePrimitiveMatrix hyperplanesTmp;
     ((R,LS),(HS,hyperplanesTmp)))


faceBuilder = (k,P) -> (
     --Checking for input errors
     if k < 0 or k > dim P then error("the codimension must be between 0 and the dimension of the polyhedron");
     if not P.cache.?faces then P.cache.faces = new MutableList;
     i := #(P.cache.faces);
     if k < i then P.cache.faces#k
     else (
	  d := dim P - k;
	  dl := P#"dimension of lineality space";
	  -- Saving the lineality space of 'P', which is the also the lineality space of each face
	  LS := P#"linealitySpace";
	  -- for d = dim P it is the polyhedron itself
	  if d == dim P then (
	       VP := vertices P;
	       RP := rays P;
	       P.cache.faces#k = {(set apply(numColumns VP, i -> VP_{i}),set apply(numColumns RP, i -> RP_{i}))};
	       P.cache.faces#k)
	  -- for k=dim(P) the faces are the vertices
	  else if d == dl then (
	       VP1 := vertices P;
	       -- Generating the list of vertices with each vertex as a polyhedron
	       apply(numColumns VP1, i -> (set {VP1_{i}},set {})))
	  else if d < dl then {}
	  else (
	       if i == 0 then (
		    VP2 := vertices P;
		    RP2 := rays P;
		    P.cache.faces#0 = {(set apply(numColumns VP2, i -> VP2_{i}),set apply(numColumns RP2, i -> RP2_{i}))};
		    i = 1);
	       if i == 1 then (
		    -- Saving the half-spaces and hyperplanes
		    (HS,v) := halfspaces P;
		    (hyperplanesTmp,w) := hyperplanes P;
		    -- Generating the list of facets where each facet is given by a list of its vertices and a list of its rays
		    Fl := apply(numRows HS, i -> intersection(HS,v,hyperplanesTmp || HS^{i},w || v^{i}));
		    Fl = apply(Fl, f -> (
			      V := vertices f;
			      R := rays f;
			      (set apply(numColumns V, i -> V_{i}),set apply(numColumns R, i -> R_{i}))));
		    i = 2;
		    P.cache.faces#1 = Fl);
	       F := P.cache.faces#1;
	       i = i - 1;
	       L := P.cache.faces#i;
	       -- Intersecting L k-1 times with F and returning the maximal inclusion sets which are the faces of codim plus 1
	       while i < k do (
		    L = intersectionWithFacets(L,F);
		    i = i+1;
		    P.cache.faces#i = L);
	       P.cache.faces#k)))


faceBuilderCone = (k,C) -> (
     d := dim C - k;
     dl := C#"dimension of lineality space";
     LS := linSpace C;
     --Checking for input errors
     if d < 0 or d > dim C then error("the codimension must be between 0 and the dimension of the cone");
     if not C.cache.?faces then C.cache.faces = new MutableList;
     i := #(C.cache.faces);
     if k < i then C.cache.faces#k
     -- for d = dim C it is the cone itself
     else if d == dim C then (
	  Rd := rays C;
	  C.cache.faces#k = {set apply(numColumns Rd, i -> Rd_{i})};
	  C.cache.faces#k)
     -- for d = dl it is the lineality space
     else if d == dl then {set {map(QQ^(ambDim C),QQ^1,0)}}
     -- for d = dl+1 it is the lineality space plus one of the rays
     else if d == dl+1 then (
	  -- Generating the list of cones given by one ray and the lineality space
	  R1 := rays C;
	  apply(numColumns R1, i -> set {R1_{i}}))
     else if 0 <= d and d < dl then {}
     else (
	  if i == 0 then (
	       R2 := rays C;
	       C.cache.faces#0 = {set apply(numColumns R2, i -> R2_{i})};
	       i = 1);
	  if i == 1 then (
	       -- Saving the half-spaces and hyperplanes
	       HS := halfspaces C;
	       hyperplanesTmp := hyperplanes C;
	       -- Generating the list of facets where each facet is given by a list of its vertices and a list of its rays
	       F1 := apply(numRows HS, i -> intersection(HS,hyperplanesTmp || HS^{i}));
	       F1 = apply(F1, f -> (
			 R := rays f;
			 (set apply(numColumns R, i -> R_{i}))));
	       i = 2;
	       C.cache.faces#1 = F1);	       
	  -- Duplicating the list of facets
	  F := C.cache.faces#1;
	  i = i-1;
	  L := C.cache.faces#i;
	  -- Intersecting L k-1 times with F and returning the maximal inclusion sets. These are the faces of codim plus 1
	  while i < k do (
	       L = intersectionWithFacetsCone(L,F);
	       i = i+1;
	       C.cache.faces#i = L);
	  -- Generating the corresponding polytopes out of the lists of vertices, rays and the lineality space
	  C.cache.faces#k))

-- PURPOSE : check whether a matrix is over ZZ or QQ
--   INPUT : '(M,msg)', a matrix 'M' and a string 'msg'
--  OUTPUT : the matrix 'M' promoted to QQ if it was over ZZ or QQ, otherwise an error
chkZZQQ = (M,msg) -> (
     R := ring M;
     if R =!= ZZ and R =!= QQ then error("expected matrix of ",msg," to be over ZZ or QQ");
     promote(M,QQ));

-- PURPOSE : check whether a matrix is over ZZ or QQ, return it over ZZ
--   INPUT : '(M,msg)', a matrix 'M' and a string 'msg'
--  OUTPUT : the matrix 'M' cleared of denominatorx columnwise and lifted to ZZ if it was over QQ, 
--     	     itself if already over ZZ, otherwise an error
chkQQZZ = (M,msg) -> (
     R := ring M;
     if R === ZZ then M else if R === QQ then makePrimitiveMatrix M else error("expected matrix of ",msg," to be over ZZ or QQ"));



-- PURPOSE : select those cones in a list that do not contain any other cone of the list
--   INPUT : 'L',  a list of cones
--  OUTPUT : The list of cones that don't contain any of the other
inclMinCones = L -> (
     newL := {};
     -- Scanning the list
     while L != {} do (
	  C := L#0;
	  L = drop(L,1);
	  -- check, if 'C' contains any cone remaining in
	  if all(L, C1 -> not contains(C,C1)) then (
	       -- if not, then check if 'C' contains any of the cones already in the final list
	       if all(newL, C1 -> not contains(C,C1)) then (
		    -- if not again, then add 'C' to the final list.
		    newL = newL | {C})));
     newL);


-- PURPOSE : intersect every face in L with every facet in F and return the inclusion maximal intersections that
--     	     are not equal to one element in L
--   INPUT : 'L',  a list of Sequences each containing a set of vertices and a set of rays giving the faces of a 
--     	    	   certain dimension of a polyhedron
--     	     'F', a list of Sequences each containing a set of vertices and a set of rays giving the facets 
--     	    	   of the same polyhedron
--  OUTPUT : a list of Sequences each containing a set of vertices and a set of rays giving the faces 
--     	    	   of the same polyhedron one dimension lower then the ones in 'L'
intersectionWithFacets = (L,F) -> (
	  -- Function to check if 'e' has at least one vertex and is not equal to 'l'
	  isValid := (e,l) -> if e#0 =!= set{} then e =!= l else false;
	  newL := {};
	  -- Intersecting each element of 'L' with each element of 'F'
	  scan(L, l -> (
		    scan(F, f -> (
			      e := ((l#0)*(f#0),(l#1)*(f#1));
			      -- if the intersection is valid add it to newL if it is not contained in one of the elements 
			      -- already in newL and remove those contained in 'e'
			      if isValid(e,l) then (
				   if not any(newL, g -> isSubset(e#0,g#0) and isSubset(e#1,g#1)) then (
					newL = select(newL, g -> not (isSubset(g#0,e#0) and isSubset(g#1,e#1)))|{e}))))));
	  newL);


-- PURPOSE : intersect every face in L with every facet in F and return the inclusion maximal intersections that
--     	     are not equal to one element in L
--   INPUT : 'L',  a list of sets each containing the rays of the faces of a certain dimension of a polyhedron
--     	     'F', a list of sets each containing the rays of the facets of the same polyhedron
--  OUTPUT : a list of sets each containing the rays of the faces of the same polyhedron one dimension lower 
--     	     then the ones in 'L'
intersectionWithFacetsCone = (L,F) -> (
	  -- Function to check if 'e' has at least one vertex and is not equal to 'l'
	  isValid := (e,l) -> if e =!= set{} then e =!= l else false;
	  newL := {};
	  -- Intersecting each element of 'L' with each element of 'F'
	  scan(L, l -> (
		    scan(F, f -> (
			      e := l*f;
			      -- if the intersection is valid add it to newL if it is not contained in one of the elements 
			      -- already in newL and remove those contained in 'e'
			     if isValid(e,l) then (
				  if not any(newL, g -> isSubset(e,g)) then (
					newL = select(newL, g -> not isSubset(g,e))|{e}))))));
	  newL);


-- PURPOSE : Computes the common refinement of a list of cones
--   INPUT : 'L',  a list of cones
--  OUTPUT : A fan, the common refinement of the cones
refineCones = L -> (
     -- Collecting the rays of all cones
     R := rays L#0;
     n := numRows R;
     R = apply(numColumns R, i -> R_{i});
     L1 := drop(L,1);
     R = unique flatten (R | apply(L1, C -> apply(numColumns rays C, i -> (rays C)_{i})));
     -- Writing the rays into one matrix
     M := matrix transpose apply(R, r -> flatten entries r);
     -- Compute the coarsest common refinement of these rays
     F := ccRefinement M;
     -- Collect for each cone of the ccRef the intersection of all original cones, that contain
     -- the interior of that cone
     fan apply(maxCones F, C -> (
	       v := interiorVector(C);
	       intersection select(L, c -> contains(c,v)))))




