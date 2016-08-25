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
   E := apply(faces(dim P -1, P), 
      e -> (
         e = e#0;
         e = vertP_e; 
         {e_{0},e_{1}}
      )
   );
   V := apply(numColumns vertP, i -> vertP_{i});
   HS := -(halfspaces P)#0;
   HS = apply(numRows HS, i -> HS^{i});
   all(V, 
      v -> (
         Ev := select(E, e -> member(v,e));
         Ev = apply(Ev, e -> makePrimitiveMatrix(if e#0 == v then e#1-e#0 else e#0-e#1));
         ind := (smithNormalForm matrix {Ev})_0;
         ind = product toList apply(rank ind, i-> ind_(i,i));
         ind == 1 or (
         EvSums := apply(subsets Ev, s -> sum(s|{v}));
         all(EvSums, e -> contains(P,e)) or (
         Ev = matrix{Ev};
         HSV := matrix for h in HS list if all(flatten entries(h*Ev), e -> e >= 0) then {h} else continue;
         CH := new HashTable from {
            rays => Ev,
            computedLinealityBasis => map(ZZ^(numRows Ev),ZZ^0,0),
            facets => HSV,
            computedHyperplanes => map(ZZ^0,ZZ^(numRows Ev),0)
         };
         C := cone CH;
         -- C := new Cone from {
         -- "ambient dimension" => numRows Ev,
         -- "dimension" => numRows Ev,
         -- "dimension of lineality space" => 0,
         -- "linealitySpace" => map(ZZ^(numRows Ev),ZZ^0,0),
         -- "number of rays" => numColumns Ev,
         -- "rays" => Ev,
         -- "number of facets" => numColumns HSV,
         -- "halfspaces" => HSV,
         -- "hyperplanes" => map(ZZ^0,ZZ^(numRows Ev),0),
         -- "genrays" => (Ev,map(ZZ^(numRows Ev),ZZ^0,0)),
         -- "dualgens" => (-(transpose HSV),map(ZZ^(numRows Ev),ZZ^0,0)),
         -- symbol cache => new CacheTable};
         HB := hilbertBasis C;
         all(HB, e -> contains(P,e+v))))
      )
   )
)


compute#Polyhedron#computedEhrhart = method(TypicalValue => RingElement)
compute#Polyhedron#computedEhrhart Polyhedron := P -> (
	n := dim P;
	v := matrix apply(n,k -> {-1+#latticePoints( (k+1)*P)});
   v = promote(v, QQ);
	M := promote(matrix apply(n,i -> reverse apply(n, j -> (i+1)^(j+1))),QQ);
	M = flatten entries ((inverse M)*v);
	R := QQ[getSymbol "x"];
	x := R_"x";
	1+sum apply(n,i -> M#i * x^(n-i))
)


compute#Polyhedron#computedPolar = method()
compute#Polyhedron#computedPolar Polyhedron := P -> (
   C := getProperty(P, underlyingCone);
   CD := dualCone C;
   result := new HashTable from {
      underlyingCone => CD
   };
   polyhedron result
)

