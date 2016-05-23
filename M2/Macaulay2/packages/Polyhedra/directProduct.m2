
-- PURPOSE : Computing the direct product of two polyhedra in the direct product of their ambient spaces
directProduct = method()

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
     intersection(M,v,N,w))


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
     intersection(M,N))


--   INPUT : '(C,P)',  a cone and a polyhedron
--  OUTPUT : A polyhedron, the direct product
directProduct (Cone,Polyhedron) := (C,P) -> directProduct(coneToPolyhedron C,P)


--   INPUT : '(P,C)',  a polyhedron and a cone
--  OUTPUT : A polyhedron, the direct product
directProduct (Polyhedron,Cone) := (P,C) -> directProduct(P,coneToPolyhedron C)


--   INPUT : '(F1,F2)',  two fans
--  OUTPUT : A fan, the direct product
directProduct (Fan,Fan) := (F1,F2) -> (
     -- computing the direct products of all pairs of generating cones
     fan flatten apply(maxCones F1, C1 -> apply(maxCones F2, C2 -> directProduct(C1,C2))))


Polyhedron * Polyhedron := directProduct
Polyhedron * Cone := directProduct
Cone * Polyhedron := directProduct
Cone * Cone := directProduct
Fan * Fan := directProduct