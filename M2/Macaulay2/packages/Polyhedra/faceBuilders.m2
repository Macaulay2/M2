---------------------------------------
-- DECLARING AUXILIARY FUNCTIONS
-- >> not public <<
---------------------------------------
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
	  LS := linSpace(P);
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
