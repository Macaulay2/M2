
Fan == Fan := (F1,F2) -> F1 === F2



--   INPUT : 'F'  a Fan
--  OUTPUT : a Matrix, where the column vectors are a basis of the lineality space
linSpace Fan := F -> linSpace((maxCones F)#0)


--   INPUT : 'F'  a Fan
rays Fan := F -> raySort toList F#"rays"


   
-- PURPOSE : Giving the generating Cones of the Fan
--   INPUT : 'F'  a Fan
--  OUTPUT : a List of Cones
maxCones = method(TypicalValue => List)
maxCones Fan := F -> maxObjects F

--   INPUT : 'F',  a Fan
--  OUTPUT : 'true' or 'false'
isPointed Fan := F -> (
     if not F.cache.?isPointed then F.cache.isPointed = isPointed((maxCones F)#0);
     F.cache.isPointed)


--   INPUT : 'F'  a Fan
--  OUTPUT : 'true' or 'false'
isSmooth Fan := F -> (
     if not F.cache.?isSmooth then F.cache.isSmooth = all(maxCones F,isSmooth);
     F.cache.isSmooth)


-- PURPOSE : Tests if a Fan is projective
--   INPUT : 'F'  a Fan
--  OUTPUT : a Polyhedron, which has 'F' as normal fan, if 'F' is projective or the empty polyhedron
isPolytopal = method(TypicalValue => Boolean)
isPolytopal Fan := F -> (
     if not F.cache.?isPolytopal then (
	  F.cache.isPolytopal = false;
	  -- First of all the fan must be complete
     	  if isComplete F then (
	       -- Extracting the generating cones, the ambient dimension, the codim 1 
	       -- cones (corresponding to the edges of the polytope if it exists)
	       i := 0;
	       L := hashTable apply(maxCones F, l -> (i=i+1; i=>l));
	       n := ambDim(F);
	       edges := cones(n-1,F);
	       -- Making a table that indicates in which generating cones each 'edge' is contained
	       edgeTCTable := hashTable apply(edges, e -> select(1..#L, j -> contains(L#j,e)) => e);
	       i = 0;
	       -- Making a table of all the edges where each entry consists of the pair of top cones corr. to
	       -- this edge, the codim 1 cone, an index number i, and the edge direction from the first to the
	       -- second top Cone
	       edgeTable := apply(pairs edgeTCTable, e -> (i=i+1; 
		    	 v := transpose hyperplanes e#1;
		    	 if not contains(dualCone L#((e#0)#0),v) then v = -v;
		    	 (e#0, e#1, i, v)));
	       edgeTCNoTable := hashTable apply(edgeTable, e -> e#0 => (e#2,e#3));
	       edgeTable = hashTable apply(edgeTable, e -> e#1 => (e#2,e#3));
	       -- Computing the list of correspondencies, i.e. for each codim 2 cone ( corresponding to 2dim-faces of the polytope) save 
	       -- the indeces of the top cones containing it
	       corrList := hashTable {};
	       scan(keys L, j -> (corrList = merge(corrList,hashTable apply(faces(2,L#j), C -> C => {j}),join)));
	       corrList = pairs corrList;
	       --  Generating the 0 matrix for collecting the conditions on the edges
	       m := #(keys edgeTable);
	       -- for each entry of corrlist another matrix is added to hyperplanesTmp
	       hyperplanesTmp := flatten apply(#corrList, j -> (
		    	 v := corrList#j#1;
		    	 hyperplanesTmpnew := map(ZZ^n,ZZ^m,0);
		    	 -- Scanning trough the top cones containing the active codim2 cone and order them in a circle by their 
		    	 -- connecting edges
		    	 v = apply(v, e -> L#e);
		    	 C := v#0;
		    	 v = drop(v,1);
		    	 C1 := C;
		    	 nv := #v;
		    	 scan(nv, i -> (
			      	   i = position(v, e -> dim intersection(C1,e) == n-1);
			      	   C2 := v#i;
			      	   v = drop(v,{i,i});
			      	   (a,b) := edgeTable#(intersection(C1,C2));
			      	   if not contains(dualCone C2,b) then b = -b;
			      	   -- 'b' is the edge direction inserted in column 'a', the index of this edge
			      	   hyperplanesTmpnew = hyperplanesTmpnew_{0..a-2} | b | hyperplanesTmpnew_{a..m-1};
			      	   C1 = C2));
		    	 C3 := intersection(C,C1);
		    	 (a,b) := edgeTable#C3;
		    	 if not contains(dualCone C,b) then b = -b;
		    	 -- 'b' is the edge direction inserted in column 'a', the index of this edge
		    	 -- the new restriction is that the edges ''around'' this codim2 Cone must add up to 0
		    	 entries(hyperplanesTmpnew_{0..a-2} | b | hyperplanesTmpnew_{a..m-1})));
	       if hyperplanesTmp != {} then hyperplanesTmp = matrix hyperplanesTmp
	       else hyperplanesTmp = map(ZZ^0,ZZ^m,0);
	       -- Find an interior vector in the cone of all positive vectors satisfying the restrictions
	       v := flatten entries interiorVector intersection(id_(ZZ^m),hyperplanesTmp);
	       M := {};
	       -- If the vector is strictly positive then there is a polytope with 'F' as normalFan
	       if all(v, e -> e > 0) then (
	       	    -- Construct the polytope
	       	    i = 1;
	       	    -- Start with the origin
	       	    p := map(ZZ^n,ZZ^1,0);
	       	    M = {p};
	       	    Lyes := {};
	       	    Lno := {};
	       	    vlist := apply(keys edgeTCTable,toList);
	       	    -- Walk along all edges recursively
	       	    edgerecursion := (i,p,vertexlist,Mvertices) -> (
		    	 vLyes := {};
		    	 vLno := {};
		    	 -- Sorting those edges into 'vLyes' who emerge from vertex 'i' and the rest in 'vLno'
		    	 vertexlist = partition(w -> member(i,w),vertexlist);
		    	 if vertexlist#?true then vLyes = vertexlist#true;
		    	 if vertexlist#?false then vLno = vertexlist#false;
		    	 -- Going along the edges in 'vLyes' with the length given in 'v' and calling edgerecursion again with the new index of the new 
		    	 -- top Cone, the new computed vertex, the remaining edges in 'vLno' and the extended matrix of vertices
		    	 scan(vLyes, w -> (
			      	   w = toSequence w;
			      	   j := edgeTCNoTable#w;
			      	   if w#0 == i then (
				   	(vLno,Mvertices) = edgerecursion(w#1,p+(j#1)*(v#((j#0)-1)),vLno,append(Mvertices,p+(j#1)*(v#((j#0)-1)))))
			      	   else (
				   	(vLno,Mvertices) = edgerecursion(w#0,p-(j#1)*(v#((j#0)-1)),vLno,append(Mvertices,p-(j#1)*(v#((j#0)-1)))))));
		    	 (vLno,Mvertices));
	       	    -- Start the recursion with vertex '1', the origin, all edges and the vertexmatrix containing already the origin
	       	    M = unique ((edgerecursion(i,p,vlist,M))#1);
	       	    M = matrix transpose apply(M, m -> flatten entries m);
	       	    -- Computing the convex hull
	       	    F.cache.polytope = convexHull M;
	       	    F.cache.isPolytopal = true)));
     F.cache.isPolytopal)


-- PURPOSE : Computing the stellar subdivision
--   INPUT : '(F,r)', where 'F' is a Fan and 'r' is a ray
--  OUTPUT : A fan, which is the stellar subdivision
stellarSubdivision = method()
stellarSubdivision (Fan,Matrix) := Fan => (F,r) -> (
     -- Checking for input errors
     if numColumns r != 1 or numRows r != ambDim F then error("The ray must be given by a one column matrix in the ambient dimension of the fan");
     divider := (C,r) -> if dim C != 1 then flatten apply(faces(1,C), f -> if not contains(f,r) then posHull {f,r} else divider(f,r)) else {C};
     L := flatten apply(maxCones F, C -> if contains(C,r) then divider(C,r) else {C});
     L = sort select(L, l -> all(L, e -> not contains(e,l) or e == l));
     n := dim L#0;
     R := unique(rays F|{promote(r,QQ)});
     new Fan from {
	  "generatingObjects" => set L,
	  "ambient dimension" => ambDim L#0,
	  "dimension" => n,
	  "number of generating cones" => #L,
	  "rays" => set R,
	  "number of rays" => #R,
	  "isPure" => dim L#0 == dim last L,
	  symbol cache => new CacheTable})



-- PURPOSE : Computing the subfan of all smooth cones of the Fan
--   INPUT : 'F',  a Fan
--  OUTPUT : The Fan of smooth cones
smoothSubfan = method(TypicalValue => Fan)
smoothSubfan Fan := F -> (
     -- recursive function that adds the cones of the list 'L' to 'F' if they are smooth
     -- and calls itself with the faces of the cone if the cone is not smooth
     facerecursion := L -> flatten apply(L, C -> if isSmooth C then C else facerecursion faces(1,C));
     L := maxCones F;
     fan facerecursion L)

-- PURPOSE : Adding a Cone to an existing fan 
--   INPUT : '(C,F)',  where 'C' is a Cone in the same ambient space as 'F'
--  OUTPUT : The original fan 'F' together with 'C' if it is compatible with the already existing cones, 
--     	     if not there is an error
addCone = method(TypicalValue => Fan)
addCone (Cone,Fan) := (C,F) -> (
     -- Checking for input errors
     if ambDim(C) != ambDim(F) then error("Cones must lie in the same ambient space");
     -- Extracting data
     GC := maxCones F;
     d := dim C;
     -- We need to memorize for later if 'C' has been inserted
     inserted := false;
     -- Cones in the list 'GC' are ordered by decreasing dimension so we start compatibility checks with 
     -- the cones of higher or equal dimension. For this we divide GC into two seperate lists
     GC = partition(Cf -> (dim Cf) >= d,GC);
     GC = {if GC#?true then GC#true else {},if GC#?false then GC#false else {}};
     if all(GC#0, Cf ->  (
	       (a,b) := areCompatible(Cf,C);
	       -- if 'Cf' and 'C' are not compatible then there is an error
	       if not a then error("The cones are not compatible");
	       -- if they are compatible and 'C' is a face of 'Cf' then 'C' does not 
	       -- need to be added to 'F'
	       b != C)) then (
	  -- otherwise 'Cf' is still a generating Cone and has to be kept and the remaining cones
	  -- have to be checked
	  GC = GC#0 | {C} | select(GC#1, Cf -> (
		    (a,b) := areCompatible(Cf,C);
		    if not a then error("The cones are not compatible");
		    -- if one of the remaining cones is a face of 'C' this Cone can be dropped
		    b != Cf));
	  inserted = true)     
     -- Otherwise 'C' was already a face of one of the original cones and does not need to be added
     else GC = flatten GC;
     -- If 'C' was added to the Fan as a generating cone then the codim 1 faces on the boundary have to changed to check for 
     -- completeness
     rayList := rays(F);
     if inserted then (
	  -- The rays of 'C' have to be added
	  rm := rays C;
	  rm = apply(numColumns rm, i -> rm_{i});
	  rayList = unique(rayList|rm));
     -- Saving the fan
     new Fan from {
	  "generatingObjects" => set GC,
	  "ambient dimension" => ambDim(F),
	  "dimension" => dim GC#0,
	  "number of generating cones" => #GC,
	  "rays" => set rayList,
	  "number of rays" => #rayList,
	  "isPure" => dim first GC == dim last GC,
	  symbol cache => new CacheTable})


--   INPUT : '(L,F)',  where 'L' is a list of Cones in the same ambient space as the fan 'F'
--  OUTPUT : The original fan 'F' together with cones in the list 'L'
addCone (List,Fan) := (L,F) -> (     
    -- Checking for input errors
    if L == {} then error("The list must not be empty");
    if (not instance(L#0,Cone)) and (not instance(L#0,Fan)) then error("The list may only contain cones and fans");
    if #L == 1 then addCone(L#0,F) else addCone(drop(L,1),addCone(L#0,F)))


--   INPUT : '(F1,F)',  where 'F1' is a fan in the same ambient space as the fan 'F'
--  OUTPUT : The original fan 'F' together with cones of the fan 'F1'
addCone (Fan,Fan) := (F1,F) -> (
     -- Checking for input errors
     if ambDim F != ambDim F1 then error("The fans must be in the same ambient space");
     L := maxCones F1;
     addCone(L,F))


-- PURPOSE : Giving the k dimensionial Cones of the Fan
--   INPUT : (k,F)  where 'k' is a positive integer and F is a Fan 
--  OUTPUT : a List of Cones
cones = method(TypicalValue => List)
cones(ZZ,Fan) := (k,F) -> objectsOfDim(k,F)



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
