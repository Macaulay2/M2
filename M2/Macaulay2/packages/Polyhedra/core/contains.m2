

-- PURPOSE : Check if 'P' contains 'Q'
--   INPUT : '(P,Q)'  two Polyhedra
--  OUTPUT : 'true' or 'false'
contains = method(TypicalValue => Boolean)
contains(Polyhedron,Polyhedron) := (P1,P2) -> (
   -- checking for input errors
   if ambDim(P1) =!= ambDim(P2) then error("Polyhedra must lie in the same ambient space");
   C1 := getProperty(P1, underlyingCone);
   C2 := getProperty(P2, underlyingCone);
   contains(C1, C2)
)


-- PURPOSE : Check if 'C1' contains 'C2'
--   INPUT : '(C1,C2)'  two Cones
contains(Cone,Cone) := (C1,C2) -> (
   -- checking for input errors
   if ambDim(C1) =!= ambDim(C2) then error("Cones must lie in the same ambient space");
   -- Saving the equations of C1 and rays of C2
   local C1ineq;
   local C1eq;
   -- Extracting inequalities of C1
   if hasProperty(C1, facets) then C1ineq = facets C1
   else if hasProperty(C1, inequalities) then C1ineq = getProperty(C1, inequalities)
   else C1ineq = facets C1;
   -- Extracting equations of C1
   if hasProperty(C1, computedHyperplanes) then C1eq = hyperplanes C1
   else if hasProperty(C1, equations) then C1eq = getProperty(C1, equations)
   else C1eq = hyperplanes C1;
   local C2rays;
   local C2lineality;
   -- Extracting rays of C2
   if hasProperty(C2, rays) then C2rays = rays C2
   else if hasProperty(C2, inputRays) then C2rays = getProperty(C2, inputRays)
   else C2rays = rays C2;
   -- Extracting lineality of C2
   if hasProperty(C2, computedLinealityBasis) then C2lineality = linealitySpace C2
   else if hasProperty(C2, inputLinealityGenerators) then C2lineality = getProperty(C2, inputLinealityGenerators)
   else C2lineality = linealitySpace C2;
   gens := C2rays | C2lineality | (-C2lineality);
   positiveTest := flatten entries (C1ineq * gens);
   zeroTest := flatten entries (C1eq * gens);
   all(positiveTest, p -> p>=0) and all(zeroTest, p -> p==0)
)

 

 
-- PURPOSE : Check if 'C' contains 'P'
--   INPUT : '(C,P)'  a Cone and a Polyhedron
contains(Cone,Polyhedron) := (C,P) -> (
      -- checking for input errors
      if ambDim(C) =!= ambDim(P) then error("Cone and Polyhedron must lie in the same ambient space");
      -- Saving the equations of C and vertices/rays of P
      M := makePrimitiveMatrix vertices(P) | rays(P);
      LS := linSpace(P);
      C1 := coneFromVData(M,LS);
      contains(C,C1))



-- PURPOSE : Check if 'P' contains 'C'
--   INPUT : '(P,C)'  a Polyhedron and a Cone
contains(Polyhedron,Cone) := (P,C) -> (
      -- checking for input errors
      if ambDim(C) =!= ambDim(P) then error("Polyhedron and Cone must lie in the same ambient space");
      -- Saving the cone 'C' as a polyhedron and using the function on two polyhedra
      Q := polyhedron C;
      contains(P,Q))



-- PURPOSE : Check if 'P' contains 'p'
--   INPUT : '(P,p)'  a Polyhedron 'P' and a point 'p' given as a matrix
contains(Polyhedron,Matrix) := (P,p) -> (
      -- checking for input errors
      if ambDim(P) =!= numRows p then error("Polyhedron and point must lie in the same ambient space");
      if numColumns p =!= 1 then error("The point must be given as a one row matrix");
      contains(P,convexHull p))



-- PURPOSE : Check if 'C' contains 'p'
--   INPUT : '(C,p)'  a Cone 'C' and a point 'p' given as a matrix
contains(Cone,Matrix) := (C,p) -> (
      -- checking for input errors
      if ambDim(C) =!= numRows p then error("Polyhedron and point must lie in the same ambient space");
      if numColumns p =!= 1 then error("The point must be given as a one row matrix");
      contains(C,convexHull p))



-- PURPOSE : Check if a list of cones 'L' contains 'C'
--   INPUT : '(L,C)'  a List of cones 'L' and a Cone 'C'
contains(List,Cone) := (L,C) -> any(L, C1 -> C1 == C)
 
 
-- PURPOSE : Check if a list of cones 'L' contains 'C'
--   INPUT : '(L,C)'  a List of cones 'L' and a Cone 'C'
contains(List,Polyhedron) := (L,P) -> any(L, Q -> Q == P)
 
 
-- PURPOSE : Check if 'F' contains 'C'
--   INPUT : '(F,C)'  a Fan 'F' and a Cone 'C'
contains(Fan,Cone) := (F,C) -> (
      -- Checking for input errors
      if ambDim F != ambDim C then error("Fan and Cone must lie in the same ambient space");
      -- Making the list of cones of same dimension as 'C'
      L := cones(dim C,F);
      contains(L,C))
