-- All the methods in this file are NOT EXPORTED.

-- THIS METHOD IS NOT EXPORTED. It is the same as in the package NormalToricVarieties
-- Given a toric divisor which is assumed to be
-- Cartier, this method returns the characters on each maximal cone which
-- determine the Cartier divisor.
cartierCoefficients = method ()
cartierCoefficients ToricDivisor := List => D ->(
  X := variety D;
  rayMatrix := matrix rays X;
  coeffs := transpose (matrix {entries D});
  apply (max X, sigma -> coeffs^sigma // rayMatrix^sigma)
);

-- THIS METHOD IS NOT EXPORTED. It is the same as in the package NormalToricVarieties
-- computes and caches the supporting hyperplanes with outer normal vectors
-- fo each cone
outerNormals = method()
outerNormals (NormalToricVariety, List) := List => (X, sigma) ->(
  if not X.cache.?outerNormals then (
    X.cache.outerNormals = new MutableHashTable
  );
  if not X.cache.outerNormals#?sigma then (
    V := transpose matrix rays X;
    (H0, H1) := fourierMotzkin V_sigma;
    X.cache.outerNormals#sigma = {H0, H1};
  );
  X.cache.outerNormals#sigma
);

-- THIS METHOD IS NOT EXPORTED. It is the same as in the package NormalToricVarieties
-- covers the pair returned by 'fourierMotzkin' into a single matrix
outerMatrix = method()
outerMatrix List := Matrix => pair ->(
  if pair#1 == 0 then transpose pair#0 else
  transpose (pair#0 | pair#1 | - pair#1)
);

-- THIS METHOD IS NOT EXPORTED. It is the same as in the package NormalToricVarieties
-- this method caches the index of a maximal cone in the target which contains
-- the image of each ray of the source.
rayMaxList = method()
rayMaxList ToricMap := List => (cacheValue symbol maxRayList) (f ->(
  X := source f;
  Y := target f;
  A := matrix f;
  maxY := max Y;
  -- find a maximal cone containing the image of each ray
  normals := new MutableHashTable;
  for ray in rays X list (
    rho := A * transpose matrix {ray};
    position(max Y, sigma ->(
        if not normals#?sigma then (
          normals#sigma = outerMatrix outerNormals(Y, sigma);
        );
        innerProducts := normals#sigma * rho;
        all(flatten entries innerProducts, b -> b <= 0)
      )
    )
  )
));

-- THIS METHOD IS NOT EXPORTED.
-- This method is essentially the same as "pullback" in ToricMaps.m2 of the package
-- NormalToricVarieties. The only difference is that it allows the input to be the
-- same type of the output of cartierCoefficients. This is needed to optimize the
-- method productMultiplicity in TropicaToricCode.m2
----------------------------------------------------------------------
-- see Thm 4.2.12.b and Prop 6.2.7 in Cox-Little-Schenck (Prop 6.1.20
-- in the preprint) note: in CLS they use inner normals, whereas we
-- use outer normals, hence the different sign
toricPullback = method()
toricPullback (ToricMap, List) := ToricDivisor => (f, cartierData) ->(
  A := matrix f;
  X := source f;
  raysX := rays X;
  maxConeIndices := rayMaxList f;
  sum for i to # raysX - 1 list (
    rho := A * transpose matrix {raysX_i};
    (transpose cartierData_(maxConeIndices_i) * rho)_(0,0) * X_i
  )
);
toricPullback (ToricMap, ToricDivisor) := ToricDivisor => (f,D) ->(
  if not isCartier D then error "-- expected a Cartier divisor";
  toricPullback(f,cartierCoefficients D);
);
