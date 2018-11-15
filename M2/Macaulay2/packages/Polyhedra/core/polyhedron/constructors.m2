-- Defining the new type Polyhedron
Polyhedron = new Type of PolyhedralObject
Polyhedron.synonym = "convex polyhedron"
globalAssignment Polyhedron

compute#Polyhedron = new MutableHashTable

Polyhedron == Polyhedron := (P1,P2) -> (
   contains(P1, P2) and contains(P2, P1)
)


internalPolyhedronConstructor = method()
internalPolyhedronConstructor HashTable := inputProperties -> (
   result := for key in keys inputProperties list(
      value := inputProperties#key;
      if instance(value,Sequence) then (
         if instance(value#0,Matrix) then (
            if (ring value#0 === ZZ) then (
               L := apply (value,v->(promote(v,QQ)));
               key => L
            ) else key => value
         ) else key => value
      ) else if instance(value,Matrix) then (
         if (ring value === ZZ) then (
            key => promote(value,QQ)
         ) else key => value
      ) else key => value
   );
   resultHash := new HashTable from result;
   constructTypeFromHash(Polyhedron, resultHash)
)


     
--   INPUT : 'P'  a Polyhedron
coneFromVData Polyhedron := P -> (
     Mrays := makePrimitiveMatrix vertices(P) | rays(P);
     Mlinspace := linSpace(P);
     coneFromVData(Mrays,Mlinspace)
)

-- PURPOSE : Computing the Convex Hull of a given set of points and rays
convexHull = method(TypicalValue => Polyhedron)

--   INPUT : 'Mvert'  a Matrix containing the generating points as column vectors
--		 'Mrays'  a Matrix containing the generating rays as column vectors
--  OUTPUT : 'P'  a Polyhedron
-- COMMENT : The description by vertices and rays is stored in P as well as the 
--           description by defining half-spaces and hyperplanes.
convexHull(Matrix, Matrix, Matrix) := (Mvert, Mrays, Mlineality) -> (
   if numgens target Mvert =!= numgens target Mrays then error ("points and rays must lie in the same space");
   if numgens target Mvert =!= numgens target Mlineality then error ("points and lineality generators must lie in the same space");
   result := new HashTable from {
      ambientDimension => numRows Mvert,
      points => Mvert,
      inputRays => Mrays,
      inputLinealityGenerators => Mlineality
   };
   internalPolyhedronConstructor result
)

convexHull(Matrix,Matrix) := (Mvert,Mrays) -> (
   r := ring Mvert;
	Mlineality := map(target Mvert,r^0,0);
   convexHull(Mvert, Mrays, Mlineality)
)


--   INPUT : 'M'  a Matrix containing the generating points as column vectors
convexHull Matrix := Mvert -> (
   r := ring Mvert;
	Mrays := map(target Mvert,r^0,0);
	convexHull(Mvert, Mrays)
)


--   INPUT : '(P1,P2)'  two polyhedra
convexHull(Polyhedron,Polyhedron) := (P1,P2) -> (
	-- Checking for input errors
	if ambDim(P1) =!= ambDim(P2) then error("Polyhedra must lie in the same ambient space");
   C1 := getProperty(P1, underlyingCone);
   C2 := getProperty(P2, underlyingCone);
   result := new HashTable from {
      ambientDimension => ambDim P1,
      underlyingCone => coneFromVData(C1, C2)
   };
   internalPolyhedronConstructor result
)
   
--   INPUT : 'L',   a list of Cones, Polyhedra, vertices given by M, 
--     	    	    and (vertices,rays) given by '(V,R)'
convexHull List := L -> (
   polyhedra := apply(L,
      l -> (
         if not instance(l, Polyhedron) then convexHull l
         else l
      )
   );
   cones := apply(polyhedra, p -> getProperty(p, underlyingCone));
   underLyingResult := coneFromVData(cones);
   result := new HashTable from {
      underlyingCone => underLyingResult
   };
   internalPolyhedronConstructor result
)


polyhedron = method()
polyhedron Cone := C -> (
   n := ambDim C;
   rayData := getSufficientRayData C;
   r := ring rayData#0;
   vertex := map(ZZ^n, ZZ^1, 0);
   convexHull(vertex, rayData#0, rayData#1)
)


polyhedronFromHData = method()
polyhedronFromHData(Matrix, Matrix) := (M,v) -> (
   r := ring M;
   Nw := map(r^0, source M, 0);
   w := map(r^0, r^1, 0);
   polyhedronFromHData(M, v, Nw, w)
)

--   INPUT : '(M,v,N,w)',  where all four are matrices (although v and w are only vectors), such
--     	    	      	  that the polyhedron is given by P={x | Mx<=v and Nx=w} 
--  OUTPUT : 'P', the polyhedron
polyhedronFromHData(Matrix,Matrix,Matrix,Matrix) := (M,v,N,w) -> (
	-- checking for input errors
	if numColumns M =!= numColumns N then error("equations of half-spaces and hyperplanes must have the same dimension");
	if numRows M =!= numRows v or numColumns v =!= 1 then error("invalid condition vector for half-spaces");
	if numRows N =!= numRows w or numColumns w =!= 1 then error("invalid condition vector for hyperplanes");
	ineq := v | M;
   ezero := matrix {flatten {1 , toList ((numgens source M):0)}};
   ineq = ineq ||  ezero;
	eq := w | N;
   result := new HashTable from {
      inequalities => (M,v),
      equations => (N,w)
   };
   internalPolyhedronConstructor result
)
