-- PURPOSE : Computing a polyhedron as the intersection of affine half-spaces and hyperplanes

--   INPUT : '(P1,P2)',  two polyhedra 
--  OUTPUT : 'P', the polyhedron that is the intersection of both
intersection(Polyhedron,Polyhedron) := {} >> o -> (P1,P2) -> (
	-- Checking if P1 and P2 lie in the same space
	if ambDim(P1) =!= ambDim(P2) then error("Polyhedra must lie in the same ambient space");
   C1 := getProperty(P1, underlyingCone);
   C2 := getProperty(P2, underlyingCone);
   C12 := intersection(C1, C2);
   result := new HashTable from {
      underlyingCone => C12
   };
   internalPolyhedronConstructor result
)


--   INPUT : '(C1,C2)',  two Cones
--  OUTPUT : 'C', the Cone that is the intersection of both
intersection(Cone,Cone) := {} >> o -> (C1,C2) -> (
	-- Checking if C1 and C2 lie in the same space
	if ambDim(C1) =!= ambDim(C2) then error("Cones must lie in the same ambient space");
	M := halfspaces C1 || halfspaces C2;
	N := hyperplanes C1 || hyperplanes C2;
	coneFromHData(M,N)
)
   
   
--   INPUT : '(C,P)',  a Cone and a Polyhedron
--  OUTPUT : 'Q', the Polyhedron that is the intersection of both
intersection(Cone,Polyhedron) := {} >> o -> (C,P) -> intersection(polyhedron C, P)



--   INPUT : '(P,C)',  a Polyhedron and a Cone
--  OUTPUT : 'Q', the Polyhedron that is the intersection of both
intersection(Polyhedron,Cone) := {} >> o -> (P,C) -> intersection(C,P)


--   INPUT : 'L',   a list of Cones, Polyhedra, other Lists and Sequences of matrices
--           Will just turn everything in the list into Polyhedra and then intersect this.
--           Works recursive.
intersection List := {} >> o -> L -> (
   L = apply(L, 
      l -> (
         if instance(l, List) or instance(l, Sequence) then intersection l
         else l
      )
   );
   result := L#0;
   for i from 1 to #L-1 do (
      result = intersection(result, L#i)
   );
   result
)

