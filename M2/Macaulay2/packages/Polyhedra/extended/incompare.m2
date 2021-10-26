

-- PURPOSE : Get the pairs of incompatible cones in a list of cones
--   INPUT : 'L',  a list of cones and fans
--  OUTPUT : 'Lpairs',  a list, empty if there is no pair of incompatible cones, otherwise it contains the pairs of cones/fans that are  
--                 	not compatible
incompCones = method(TypicalValue => List)
incompCones List := L -> (
     if any(L, l -> (not instance(l,Cone)) and (not instance(l,Fan))) then error("The list may only contain cones and fans");
     select(apply(subsets(L,2),toSequence), p -> not commonFace p))


--   INPUT : '(C,F)',  a cone and a fan
--  OUTPUT : 'Lpairs',  a list, empty if there is no pair of incompatible cones, otherwise it contains the pairs of 'C' with the cones of 
--                 	'F' that are not compatible
incompCones(Cone,Fan) := (C,F) -> select(apply(values getProperty(F, honestMaxObjects), f -> (C,f)), p -> not commonFace p)


--   INPUT : '(F,C)',  a fan and a cone
--  OUTPUT : 'Lpairs',  a list, empty if there is no pair of incompatible cones, otherwise it contains the pairs of 'C' with the cones of 
--                 	'F' that are not compatible
incompCones(Fan,Cone) := (F,C) -> 
   select(
      apply(values getProperty(F, honestMaxObjects), 
         f -> (f,C)
      ), 
      p -> 
         not commonFace p
      )


--   INPUT : '(F1,F2)',  two fans
--  OUTPUT : 'Lpairs',  a list, empty if there is no pair of incompatible cones, otherwise it contains the pairs of cones of 'F1' and cones of 
--                 	'F2' that are not compatible
incompCones(Fan,Fan) := (F1,F2) -> 
   flatten apply(values getProperty(F1, honestMaxObjects), 
      C1 -> flatten apply(values getProperty(F2, honestMaxObjects),
         C2 -> if not commonFace(C1,C2) then (C1,C2) else {}
      )
   )


-- PURPOSE : Get the pairs of incompatible polyhedra in a list of polyhedra
--   INPUT : 'L',  a list of polyhedra and polyhedral complexes
--  OUTPUT : 'Lpairs',  a list, empty if there is no pair of incompatible polyhedra, otherwise it contains the pairs of polyhedra/polyhedral 
--                      complexes that are not compatible
incompPolyhedra = method(TypicalValue => List)
incompPolyhedra List := L -> (
     if any(L, l -> (not instance(l,Polyhedron)) and (not instance(l,PolyhedralComplex))) then error("The list may only contain polyhedra and polyhedral complexes");
     select(apply(subsets(L,2),toSequence), p -> not commonFace p))


--   INPUT : '(P,PC)',  a Polyhedron and a PolyhedralComplex
--  OUTPUT : 'Lpairs',  a list, empty if there is no pair of incompatible polyhedra, otherwise it contains the pairs of 'P' with the polyhedra of 
--                 	'PC' that are not compatible
incompPolyhedra(Polyhedron,PolyhedralComplex) := (P,PC) -> select(apply(getProperty(PC, honestMaxObjects), p -> (P,p)), e -> not commonFace e)


--   INPUT : '(PC,P)',  a PolyhedralComplex and a Polyhedron
--  OUTPUT : 'Lpairs',  a list, empty if there is no pair of incompatible polyhedra, otherwise it contains the pairs of 'P' with the polyhedra of 
--                 	'PC' that are not compatible
incompPolyhedra(PolyhedralComplex,Polyhedron) := (PC,P) -> select(apply(getProperty(PC, honestMaxObjects), p -> (p,P)), e -> not commonFace e)


--   INPUT : '(PC1,PC2)',  two PolyhedralComplexes
--  OUTPUT : 'Lpairs',  a list, empty if there is no pair of incompatible polyhedra, otherwise it contains the pairs of polyhedra of 'PC1' and polyhedra of 
--                 	'PC2' that are not compatible
incompPolyhedra(PolyhedralComplex,PolyhedralComplex) := (PC1,PC2) -> flatten apply(getProperty(PC1, honestMaxObjects), P1 -> flatten apply(getProperty(PC2, honestMaxObjects), P2 -> if not commonFace(P1,P2) then (P1,P2) else {}))
     
