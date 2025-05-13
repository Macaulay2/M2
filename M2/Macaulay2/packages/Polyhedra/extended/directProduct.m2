
-- PURPOSE : Computing the direct product of two polyhedra in the direct product of their ambient spaces

--   INPUT : '(P,Q)',  two polyhedra
--  OUTPUT : A polyhedron, the direct product
directProduct (Polyhedron,Polyhedron) := (P,Q) -> (
     -- Extracting half-spaces and hyperplanes of P and Q
     (Mp,vp) := halfspaces P;
     (Np,wp) := hyperplanes P;
     (Mq,vq) := halfspaces Q;
     (Nq,wq) := hyperplanes Q;
     -- Constructing the new half-spaces matrix |Mp 0 | and vector |vp|
     --                                        |0  Mq|            |vq|
     M := Mp ++ Mq;
     v := vp || vq;
     -- Constructing the new hyperplanes matrix |Np 0 | and vector |wp|
     --                                         |0  Nq|            |wq|
     N := Np ++ Nq;
     w := wp || wq;
     polyhedronFromHData(M,v,N,w))


--   INPUT : '(C1,C2)',  two cones
--  OUTPUT : A cone, the direct product
directProduct (Cone,Cone) := (C1,C2) -> (
     -- Extracting half-spaces and hyperplanes of P and Q
     Mp := halfspaces C1;
     Np := hyperplanes C1;
     Mq := halfspaces C2;
     Nq := hyperplanes C2;
     -- Constructing the new half-spaces matrix |Mp 0 |
     --                                        |0  Mq|
     M := Mp ++Mq;
     -- Constructing the new hyperplanes matrix |Np 0 |
     --                                         |0  Nq|
     N := Np ++ Nq;
     coneFromHData(M,N))


--   INPUT : '(C,P)',  a cone and a polyhedron
--  OUTPUT : A polyhedron, the direct product
directProduct (Cone,Polyhedron) := (C,P) -> directProduct(polyhedron C,P)


--   INPUT : '(P,C)',  a polyhedron and a cone
--  OUTPUT : A polyhedron, the direct product
directProduct (Polyhedron,Cone) := (P,C) -> directProduct(P,polyhedron C)


--   INPUT : '(F1,F2)',  two fans
--  OUTPUT : A fan, the direct product
directProduct (Fan,Fan) := (F1,F2) -> (
-- computing the direct products of all pairs of generating cones
   resultRays := rays F1 | map(ZZ^(numRows rays F1), ZZ^(numColumns rays F2), 0); 
   resultRays = resultRays || (map(ZZ^(numRows rays F2), ZZ^(numColumns rays F1), 0) | rays F2);
   mc1 := maxCones F1;
   mc2 := maxCones F2;
   shift := numColumns rays F1;
   mc2 = apply(mc2,
      m -> (
         apply(m, e -> e + shift)
      )
   );
   resultCones := apply(mc1,
      m1 -> (
         apply(mc2, m2 -> flatten {m1, m2})
      )
   );
   fan(resultRays, flatten resultCones)
)


Polyhedron * Polyhedron := directProduct
Polyhedron * Cone := directProduct
Cone * Polyhedron := directProduct
Cone * Cone := directProduct
Fan * Fan := directProduct
