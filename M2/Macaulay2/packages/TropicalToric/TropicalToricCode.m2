--input: a normal toric variety X,
--       a balanced pure dimensional fan T with a refinement that is a subfan of X,
--       a list l of weights of the maximal cones of T.
--output: list of weights, indexed by the cones of X, induced by T
refineMultiplicity = method(TypicalValue => List)
refineMultiplicity (List, Fan, NormalToricVariety) := (l,T,X) ->(
  k := dim T;
  n := dim X;
  if #(unique l) == 1 then(
    return toList(#(orbits(X,n-k)):l_0);
  );
  maxConesT := maxCones T;
  mult := {};
  for sigma in orbits(X,n-k) do(
    j := 0;
    coneSigma := coneFromVData (transpose matrix rays X)_sigma;
    --Find a maximal cone in T that contains the cone sigma
    for i from 0 to (#maxConesT)-1 when j == 0 do(
      coneT := coneFromVData (rays T)_(maxConesT_i);
      if contains(coneT, coneSigma) then(
        j = l_i;
      );
    );
    mult = append(mult,j);
  );
  return mult
);
refineMultiplicity (TropicalCycle, NormalToricVariety) := (T,X) ->(
  return refineMultiplicity(multiplicities T,fan T,X)
)

--input: normal toric varieties X,X' such that the identity on the lattices induces
--       a toric map phi:X' -> X,
--       list mult of multiplicities of cones of X' of dimension k
--output: list of degrees deg([Y'] * phi^*(V(sigma)), where [Y'] is the class of the cycle
--        Y' in X' corresponding to the Minkowski weight given by mult.
-- Note that here [Y'] * V(sigma') = mult_sigma' for every cone sigma' of X'.
-- From the projection formula, the resulting Minkowski weight on X corresponds
-- to the pushforward phi_* [Y']
pushforwardMultiplicity = method(TypicalValue => List)
pushforwardMultiplicity (NormalToricVariety,NormalToricVariety,List,ZZ) := (X,X',mult,k) ->(
  n := dim X;
  d := #rays X;
  phi := map(X,X',id_(ZZ^n));
  rayMatrix := matrix rays X;
  L := new List; --list of pullbacks phi*(X_i)
  for i from 0 to d-1 do(
    cartierData := apply (max X, sigma ->
      if member(i,sigma) then(
        (id_(ZZ^d))_{i}^sigma // rayMatrix^sigma
      ) else(
        matrix 0_(ZZ^n)
      )
    ); --this is faster than cartierCoefficients(X_i)
    L = append(L, toricPullback(phi,cartierData));
  ); --this is faster than L := apply(#rays X, i->pullback(phi,X_i) );
  prod := new List;
  for sigma in orbits(X,n-k) do(
    P := toricCycle( product L_sigma ); --pullback of V(sigma)
    --now compute [Y'] * pullback of V(sigma), substituting [Y'] * V(sigma') = m(sigma')
    prod = append( prod, sum apply(#mult, i->P_((orbits(X',n-k))_i) * mult_i) );
  );
  prod
);

--input: smooth toric variety X, integer k
--output: the matrix of products V(sigma)*V(tau), with dim(sigma) = k, dim(tau) = m-k,
--        where m is the largest integer such that A^m(X) is not zero.
--        The output is cached in X
poincareMatrix = method (TypicalValue => Matrix)
poincareMatrix (NormalToricVariety,ZZ) := (X,k) ->(
  if X.cache#?(PoincareMatrix,k) then(
    return X.cache#(PoincareMatrix,k);
  );
  if not(isSmooth X) then (
    error ("--  the toric variety is not smooth")
  );
  n := dim X;
  m := #(max X)_0;
  M := {}; --matrix ( V(tau) * V(sigma) ) with dim(sigma) = k, dim(tau) = m-k
  --note that the dimension of the cones in orbits(X,i) is n-i
  for sigma in orbits(X,n-k) do(
    M' := {};
    for tau in orbits(X,n-(m-k)) do(
      C := product( {X_sigma} | (tau / (i->X_i)) );
      if support C == {} then (
        M' = append(M',0);
      )
      else (
        -- the cycles V(sigma') with dim sigma' = m are all equivalent
        M' = append(M', sum apply(support C, c -> C#c));
      )
    );
    M = append(M,M');
  );
  M = promote(matrix M,QQ);
  X.cache#(symbol PoincareMatrix, k) = M;
  M
);

--input: smooth normal toric variety X, list of integers l representing
--       a function in Hom(A^k(X),Z)
--output: the corresponding class in A^(m-k)(X) by Poincare duality,
--        where m is the maximum codimension such that A^m(X) is not zero
--        ( recall that A^k(X) corresponds to orbits(X,n-k) )
poincareDuality = method(TypicalValue => List)
poincareDuality (List,NormalToricVariety,ZZ) := (l, X, k) ->(
  M := poincareMatrix(X,k);
  v := promote(transpose matrix {l},QQ);
  Y := flatten entries transpose solve(M,v)
);

--input: A simplicial toric variety X, an ideal I in the Laurent polynomial ring
--output: a toric cycle whose class is the class of the closure of V(I) in X
classFromTropical = method(TypicalValue => ToricCycle)
classFromTropical (NormalToricVariety,Ideal) := (X, I) ->(
  T := tropicalVariety(I);
  k := dim T;
  F := gfanFanCommonRefinement(fan X, fan T); --common refinement
  X' := makeSimplicial (normalToricVariety F);
  --ordered vector of multiplicities with respect to fan X'
  mult := refineMultiplicity(T,X');
  --vector of products [V(J)]*[V(sigma)] in X
  prod := pushforwardMultiplicity(X,X',mult,k);
  --apply Poincare duality
  Y := poincareDuality(prod,X,k);
  m := #(max X)_0;
  O := orbits(X,dim X-(m-k));
  D := sum apply(#O, s -> Y_s * X_(O_s));
  return D
);

--input: A linear ideal I in the Laurent polynomial ring
--output: a toric cycle whose class is the class of the
--        closure of V(I+J) in a wonderful compactification of V(I)
classWonderfulCompactification = method(TypicalValue => ToricCycle)
classWonderfulCompactification (NormalToricVariety, Ideal, Ideal) := (X,I,J) ->(
  if not(degree(I) == 1) then
    error("-- the ideal is not linear!");
  m := dim fan X;
  n := dim X;
  T := tropicalVariety(I+J);
  k := dim T;
  X':= makeSimplicial (normalToricVariety fan T); --no need to do the common refinement
  mult := for C in orbits(X',(dim X)-k) list(
    (multiplicities T)_(position(maxCones T, c -> isSubset(C,c)))
  ); --faster than mult := refineMultiplicity(T,X');
  prod := pushforwardMultiplicity(X,X',mult,k);
  --apply Poincare duality
  Y := poincareDuality(prod,X,k);
  O := orbits(X,n-m+k); --we want the cycle to have codimension inside X the same that T has inside T'
  D := sum apply(#O, s -> Y_s * X_(O_s));
  return D
);
classWonderfulCompactification (NormalToricVariety, Ideal, RingElement) := (X,I,f) ->(
  return classWonderfulCompactification(X,I,ideal(f))
);
classWonderfulCompactification (Ideal, Ideal) := (I,J) ->(
  X := normalToricVariety fan tropicalVariety I;
  return classWonderfulCompactification(X,I,J)
);
classWonderfulCompactification (Ideal, RingElement) := (I,f) ->(
  return classWonderfulCompactification(I,ideal(f))
);

--input: Normal toric variety X, homogeneous ideal I of the Cox ring of X
--output: (saturated) ideal J of the intersection of V(I) with the torus inside X
--
-- Let S = C[x_0 .. x_n] be the Cox ring of X, and let R be the Laurent ring of the torus inside X.
-- Then we have  R \simeq (S_(x_0*...*x_n))_0
-- here we are viewing the torus T in X as X \ V(x_0*...*x_n) = X \ (union of D_i's)
torusIntersection = method()
torusIntersection (NormalToricVariety,RingElement,Ring) := (X,g,TR) ->(
  n := dim X;
  R := matrix rays X;
  l := new List;
  expg := exponents g;
  --apply the isomorphism from the Cox ring to the torus ring, working on the exponents
  for m in expg do(
    --dehomogenize dividing by the first monomial of g
    v := transpose matrix {m - (expg_0)};
    --write each monomial as a product of "variables of degree 0"
    l = append( l, flatten entries transpose solve(R,v) );
  );
  --clear denominators
  l = transpose l;
  L := new List;
  for r in l do (
    L = append( L, toList(#(l_0):min(r)) );
  );
  l = transpose (l-L);
  --from the exponents to the polynomial
  f := sum apply(#l, i ->
    ((listForm g)_i)_1 * product apply( n, j ->(TR_j)^((l_i)_j) )
  );
  return f
);
torusIntersection (NormalToricVariety,RingElement) := (X,g) ->(
  n := dim X;
  y := symbol y;
  TR:= QQ[y_1..y_n]; --torus ring
  return torusIntersection(X,g,TR)
);
torusIntersection (NormalToricVariety,Ideal,Ring) := (X,I,TR) ->(
  G := new List;
  for g in flatten entries (gens I) do(
    G = append(G, torusIntersection(X,g,TR));
  );
  G = ideal G;
  --saturate the ideal with respect to the product of variables in TR
  for a in gens TR do(
    G = saturate(G,a);
  );
  return G
);
torusIntersection (NormalToricVariety,Ideal) := (X,I) ->(
  n := dim X;
  y := symbol y;
  TR:= QQ[y_1..y_n]; --torus ring
  return torusIntersection(X,I,TR)
);

--input: A smooth toric variety X, an ideal I in the Cox ring of X
--output: a toric cycle whose class is the class of V(I) in X
classFromTropicalCox = method(TypicalValue => RingElement)
classFromTropicalCox (NormalToricVariety,Ideal) := (X,I) -> classFromTropical(X, torusIntersection(X,I))
