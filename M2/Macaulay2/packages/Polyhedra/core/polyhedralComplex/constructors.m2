-- Defining the new type PolyhedralComplex
PolyhedralComplex = new Type of PolyhedralObject
globalAssignment PolyhedralComplex

compute#PolyhedralComplex = new MutableHashTable;


polyhedralComplex = method()
polyhedralComplex HashTable := inputProperties -> (
   constructTypeFromHash(PolyhedralComplex, inputProperties)
)


polyhedralComplex(Matrix, Matrix, Matrix, List) := (V, R, lineality, mO) -> (
   if not all(mO, m -> #m == 2) then error("Expected pairs of lists.");
   shift := numColumns V;
   mO = apply(mO,
      m -> (
         flatten{m#0, apply(m#1, e->e+shift)}
      )
   );
   irays := prependOnes(V) | prependZeros(R);
   ifanHash := new HashTable from {
      inputRays => irays, 
      computedLinealityBasis => prependZeros(lineality),
      inputCones => mO
   };
   result := new HashTable from {
      underlyingFan => fan ifanHash
   };
   polyhedralComplex result
)


polyhedralComplex(Matrix, List) := (V, mO) -> (
   if not all(mO, m-> all(m, e->instance(e, List))) then (
      mO = apply(mO, m -> {m, {}})
   );
   RL := map(QQ^(numRows V), QQ^0, 0);
   polyhedralComplex(V, RL, RL, mO)
)


polyhedralComplex Polyhedron := P -> (
   C := getProperty(P, underlyingCone);
   result := new HashTable from {
      underlyingFan => fan C
   };
   polyhedralComplex result
)


polyhedralComplex List := L -> (
   if not all(L, l->instance(l, Polyhedron) or instance(l, PolyhedralComplex)) then error("Expected list of polyhedra and polyhedral complices.");
   PCs := select(L, l->instance(l, PolyhedralComplex));
   Ps := select(L, l->instance(l, Polyhedron));
   PCs = flatten apply(PCs, l->getProperty(l, honestMaxObjects));
   MO := flatten {Ps, PCs};
   result := polyhedralComplex MO#0;
   addPolyhedron(MO, result)
)


addPolyhedron = method()
addPolyhedron(Polyhedron,PolyhedralComplex) := (P, PC) -> (
   F := getProperty(PC, underlyingFan);
   C := getProperty(P, underlyingCone);
   result := new HashTable from {
      underlyingFan => addCone(F, C)
   };
   polyhedralComplex result
)


addPolyhedron(List,PolyhedralComplex) := (L, PC) -> (
   if not all(L, p -> instance(p, Polyhedron)) then error("Expected list of polyhedra.");
   result := PC;
   for p in L do (
      result = addPolyhedron(p, result)
   );
   result
)
