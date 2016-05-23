-- Defining the new type Fan
Fan = new Type of PolyhedralObjectFamily
globalAssignment Fan


--   INPUT : 'F'  a Fan
--  OUTPUT : a Matrix, where the column vectors are a basis of the lineality space
linSpace Fan := F -> ((maxCones F)#0)#"linealitySpace"


--   INPUT : 'F'  a Fan
rays Fan := F -> raySort toList F#"rays"


   
-- PURPOSE : Giving the generating Cones of the Fan
--   INPUT : 'F'  a Fan
--  OUTPUT : a List of Cones
maxCones = method(TypicalValue => List)
maxCones Fan := F -> toList F#"generatingObjects"

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
	       n := F#"ambient dimension";
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
