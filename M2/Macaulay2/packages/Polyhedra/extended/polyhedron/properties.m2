compute#Polyhedron#computedNormal = method()
compute#Polyhedron#computedNormal Polyhedron := P -> (
   if not isCompact P then error ("The polyhedron must be compact");
   C := getProperty(P, underlyingCone);
   L := hilbertBasis C;
   n := ambDim P;
   -- Do all lattice points lie in height one?
   all(L,v -> v_(0,0) == 1)
)


compute#Polyhedron#computedVeryAmple = method()
compute#Polyhedron#computedVeryAmple Polyhedron := P -> (
   if not isCompact P then error("The polyhedron must be compact");
   if not dim P == ambDim P then error("The polyhedron must be full dimensional");
   if not isLatticePolytope P then error("The polyhedron must be a lattice polytope");
   vertP := vertices P;
   edgesP := apply(faces(dim P -1, P), 
      e -> (
         e = e#0;
         e = vertP_e; 
         {e_{0},e_{1}}
      )
   );
   vertP = apply(numColumns vertP, i -> vertP_{i});
   HS := -(halfspaces P)#0;
   HS = apply(numRows HS, i -> HS^{i});
   all(vertP, 
      source -> (
         outgoingEdges := select(edgesP, e -> member(source,e));
         outgoingEdges = apply(outgoingEdges, e -> makePrimitiveMatrix(if e#0 == source then e#1-e#0 else e#0-e#1));
         ind := (smithNormalForm matrix {outgoingEdges})_0;
         ind = product toList apply(rank ind, i-> ind_(i,i));
         ind == 1 or (
         outgoingEdgesSums := apply(subsets outgoingEdges, s -> sum(s|{source}));
         all(outgoingEdgesSums, e -> contains(P,e)) or (
            outgoingEdges = matrix{outgoingEdges};
            HSV := matrix for h in HS list if all(flatten entries(h*outgoingEdges), e -> e >= 0) then {h} else continue;
            CH := new HashTable from {
               rays => outgoingEdges,
               computedLinealityBasis => map(ZZ^(numRows outgoingEdges),ZZ^0,0),
               facets => HSV,
               computedHyperplanes => map(ZZ^0,ZZ^(numRows outgoingEdges),0)
            };
            C := internalConeConstructor CH;
            HB := hilbertBasis C;
            all(HB, e -> contains(P,e+source)))
         )
      )
   )
)


compute#Polyhedron#computedEhrhart = method(TypicalValue => RingElement)
compute#Polyhedron#computedEhrhart Polyhedron := P -> (
	n := dim P;
	R := QQ[getSymbol "x"];
	x := R_"x";
   if n==0 and (not isEmpty P) then return 1 + 0*x;
   if isEmpty P then return 0 + 0*x;
	v := matrix apply(n,k -> {-1+#latticePoints( (k+1)*P)});
   v = promote(v, QQ);
	M := promote(matrix apply(n,i -> reverse apply(n, j -> (i+1)^(j+1))),QQ);
	M = flatten entries ((inverse M)*v);
	1+sum apply(n,i -> M#i * x^(n-i))
)


compute#Polyhedron#computedPolar = method()
compute#Polyhedron#computedPolar Polyhedron := P -> (
   C := getProperty(P, underlyingCone);
   CD := dualCone C;
   result := new HashTable from {
      underlyingCone => CD
   };
   internalPolyhedronConstructor result
)


compute#Polyhedron#latticeVolume = method()
compute#Polyhedron#latticeVolume Polyhedron := P -> (
   d := dim P;
   if isEmpty P then return 0;
   -- Checking for input errors
   if  not isCompact P then error("The polyhedron must be compact, i.e. a polytope.");
   -- If P is not full dimensional then project it down
   if d != ambDim P then (
      A := substitute((hyperplanes P)#0,ZZ);
      A = inverse (smithNormalForm A)#2;
      n := ambDim P;
      A = A^{n-d..n-1};
      P = affineImage(A,P);
      d = dim P;
   );
   if d == 0 and (not isEmpty P) then return 1;
   volumeFromTriangulation(P, d)
)

volumeFromTriangulation = method()
volumeFromTriangulation(Polyhedron, ZZ) := (P, d) -> (
   -- Computing the triangulation of P
   T := regularTriangulation P;
   -- Computing the volume of each simplex without the dimension factor, by 
   -- taking the absolute of the determinant of |v_1-v_0..v_d-v_0|
   V := vertices P;
   simplexVolumes := apply(T, 
      p -> (
         if #p == 0 then 1
         else abs det matrix transpose apply(toList(1..d), i -> flatten entries(V_(p#i) - V_(p#0)))
      )
   );
   -- Summing up the volumes
   (sum simplexVolumes)
)

