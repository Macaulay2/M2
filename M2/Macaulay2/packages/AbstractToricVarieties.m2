-- TODO:
--   (1) need subToricVariety, and resulting Schubert2 notions: integral, pullback, pushforward, etc.
--   why not just use sectionZeroLocus?  Because we get more information, including the fan.
--   and the map can be computed without doing a ideal quotient.
--   (2) examples, doc, tests.  especially, what is the point of abstractSheaf here?
newPackage(
        "AbstractToricVarieties",
        Version => "0.1", 
        Date => "10 June 2017",
        Authors => {
            {Name => "Mike Stillman", 
                Email => "mike@math.cornell.edu", 
                HomePage => "http://www.math.cornell.edu/~mike"}
            },
        Headline => "links abstract simplicial (normal) toric varieties to Schubert2",
	Keywords => {"Toric Geometry", "Intersection Theory"},
        PackageExports => {"NormalToricVarieties", "Schubert2"}
        )

export {    
    "abstractPrimeToricDivisor",
    "intersectionRingIdeal",
    "completeIntersection",
    "CompleteIntersectionInToric",
        "Ambient",
        "CI",
    "liftPicToCDiv", -- these might now be in NormalNoricVarieties?
    "liftClToWDiv"
    }

  liftPicToCDiv = method()
  liftPicToCDiv NormalToricVariety := (X) -> (
      A := fromCDivToPic X;
      IX := intersectionRing X;
      bas := (flatten entries basis(1, IX))/index;
      if not abs det A_bas == 1 then error "I thought this would be invertible over ZZ...";
      B := inverse(A_bas);
      if not B * A_bas == 1 then error "didn't get inverse matrix";
      result := mutableMatrix(ZZ, numColumns A, numRows A);
      for b from 0 to #bas-1 do (
          for i from 0 to numColumns result - 1 do result_(bas#b,i) = B_(b,i);
          );
      matrix result
      )  
  
  liftClToWDiv = method()
  liftClToWDiv NormalToricVariety := (X) -> (
      A := fromWDivToCl X;
      I := id_(ZZ^(numRows A));
      if I % A != 0 then error "my logic is wrong, there is no lift from Cl to WDiv??";
      I // A
      )

  intersectionRing NormalToricVariety := (X) -> (
      if not isSimplicial X then error "intersection ring for non-simplicial toric varieties not yet implemented";
      if isSimplicial X then (
          S := ring X;
          n := numgens S;
          t := getSymbol "t";
          R := QQ(monoid [t_0 .. t_(n-1)]);
          phi := map(R,S,gens R);
          M := phi dual monomialIdeal X;
          L := ideal(vars R * (matrix rays X) ** R);
          R/(M+L)
          )
      )

  intersectionRingIdeal = method(Options => {CoefficientRing=>QQ});
  intersectionRingIdeal NormalToricVariety := opts -> (X) -> (
      kk := opts.CoefficientRing;
      if not isSimplicial X then error "intersection ring for non-simplicial toric varieties not yet implemented";
      if not X.cache#?(intersectionRingIdeal, kk) then
        X.cache#(intersectionRingIdeal, kk) = (
          n := # rays X;
          t := getSymbol "t";
          R := kk(monoid [t_0 .. t_(n-1), Join=>false]);
          -- Problem: the code below won't work well if kk is not well-behaved?
          B := monomialIdeal apply(max X, 
      	    L -> product(n, i -> if member(i,L) then 1_R else R_i));
          M := dual B;
          L := ideal(vars R * matrix rays X);
          M + L
          );
      X.cache#(intersectionRingIdeal, kk)
      )

  intersectionRing NormalToricVariety := (X) -> (
      I := intersectionRingIdeal X;
      (ring I)/I
      )

  intersectionRing(NormalToricVariety, Ring) := (X, kk) -> (
      I := intersectionRingIdeal(X, CoefficientRing=>kk);
      (ring I)/I
      )

  abstractVariety(NormalToricVariety,AbstractVariety) := opts -> (Y,B) -> (
      if not isSimplicial Y then error "abstract variety for non-simplicial toric varieties not yet implemented";
      if not Y.cache#?(abstractVariety, B) then Y.cache#(abstractVariety,B) = (
          kk := intersectionRing B;
          elapsedTime IY := intersectionRing(Y, kk);
          --amb := kk[gens ambient IY, Join=>false];
          --IY = amb/(sub(ideal IY, vars amb));
          aY := abstractVariety(dim Y, IY);
          aY.TangentBundle = abstractSheaf(aY, Rank=>dim Y, ChernClass => product(gens IY, x -> 1+x));
          -- Now we determine the mapping 'integral':
          raysY := transpose matrix rays Y;
          onecone := first max Y;
          pt := (abs det raysY_onecone) * product(onecone, i -> IY_i);
          if size pt != 1 then error "cannot define integral: some strange error has occurred";
          mon := leadMonomial pt;
          cf := leadCoefficient pt;
          if not liftable(cf, QQ) then error "cannot create integral function";
          a := 1 / lift(cf, QQ);
          integral IY := (f) -> a * coefficient(mon, f);
          -- a check:
            --print for f in max Y list product(f, i -> IY_i);
            assert all(max Y, f -> integral(product(f, i -> IY_i)) == 1/(abs(det raysY_f)));
          aY
          );
      Y.cache#(abstractVariety, B)
      )
  abstractVariety NormalToricVariety := opts -> (Y) -> abstractVariety(Y, point)

  -- create an abstract variety which is a codimension one
  -- toric subvariety of a toric subvariety.
  -- This is a bit HACKED UP: the reason is that for some examples,
  -- the computation of the intersection ring of Y is too expensive,
  -- yet the intersection ring of a codimension one toric subvariety
  -- of Y is not so bad.
  abstractPrimeToricDivisor = method(Options => options abstractVariety)
  abstractPrimeToricDivisor(NormalToricVariety,AbstractVariety,ZZ) := opts -> (Y,B,rho) -> (
      if not isSimplicial Y then error "abstract variety for non-simplicial toric varieties not yet implemented";
      if not Y.cache#?(abstractVariety, B, rho) then Y.cache#(abstractVariety, B, rho) = elapsedTime (
          kk := intersectionRing B;
          JY := intersectionRingIdeal(Y, CoefficientRing => kk);
          M := monomialIdeal select(JY_*, f -> size f == 1);
          L := ideal select(JY_*, f -> size f > 1);
          JD := (M : (ring JY)_rho) + L;
          ID := (ring JD)/JD;
          aD := abstractVariety(dim Y-1, ID);
          aD.TangentBundle = abstractSheaf(aD, Rank=>dim Y-1, ChernClass => product(gens ID, x -> 1+x));
          -- Now we determine the mapping 'integral':
          raysY := transpose matrix rays Y;
          onecone := select(1, max Y, s -> member(rho,s));
          assert(#onecone == 1);
          onecone = first onecone;
          onecone' := sort toList((set onecone) - (set {rho}));
          pt := (abs det raysY_onecone) * product(onecone', i -> ID_i);
          if size pt != 1 then error "cannot define integral: some strange error has occurred";
          mon := leadMonomial pt;
          cf := leadCoefficient pt;
          if not liftable(cf, QQ) then error "cannot create integral function";
          a := 1 / lift(cf, QQ);
          integral ID := (f) -> a * coefficient(mon, f);
          -- a check:
            --maxD := for sigma in max Y list if member(rho,sigma) then (sigma, sort toList(set sigma - set {rho})) else continue;
            --assert all(maxD, f -> integral(product(f#1, i -> ID_i)) == 1/(abs(det raysY_(f#0))));
          aD
          );
      Y.cache#(abstractVariety, B, rho)
      )

  abstractSheaf(NormalToricVariety, AbstractVariety, ToricDivisor) := ops -> (Y,B,D) -> (
      aY := abstractVariety(Y,B);
      A := intersectionRing aY;
      OO (((vars A) * vector D)_0)
      )
  abstractSheaf(NormalToricVariety, AbstractVariety, CoherentSheaf) := opts -> (Y,B,F) -> (
      if variety F =!= Y then error "expected sheaf over the same variety";
      M := F.module;
      if not isFreeModule M then error "expected a locally free sheaf presented as a free module";
      aY := abstractVariety(Y,B);
      A := intersectionRing aY;
      D := liftClToWDiv Y;
      degs := (vars A) * (D * transpose matrix degrees M);
      degs = flatten entries degs;
      lbundles := degs/(d -> OO (-d));
      ans := lbundles_0;
      for i from 1 to #lbundles-1 do ans = ans ++ lbundles_i;
      ans
      )

--------------------------------------------------------
-- Code for complete intersections in toric varieties --
-- more functionality is in CohomCalg.m2              --
--------------------------------------------------------
CompleteIntersectionInToric = new Type of HashTable

completeIntersection = method()
completeIntersection(NormalToricVariety, List) := (Y,CIeqns) -> (
    if not all(CIeqns, d -> instance(d, ToricDivisor))
    then error "expected a list of toric divisors";
    if not all(CIeqns, d -> variety d === Y)
    then error "expected a list of toric divisors on the given toric variety";
    new CompleteIntersectionInToric from {
        symbol Ambient => Y,
        symbol CI => CIeqns,
        symbol cache => new CacheTable
        }
    )
dim CompleteIntersectionInToric := (X) -> dim X.Ambient - #X.CI
ambient CompleteIntersectionInToric := (X) -> X.Ambient

abstractVariety(CompleteIntersectionInToric, AbstractVariety) := opts -> (X,B) -> (
    if not X.cache#?(abstractVariety, B) then X.cache#(abstractVariety, B) = (
        aY := abstractVariety(ambient X, B);
        -- Question: how best to define F??
        bundles := X.CI/(d -> OO d);
        F := bundles#0;
        for i from 1 to #bundles-1 do F = F ++ bundles#i;
        aF := abstractSheaf(ambient X, B, F);
        sectionZeroLocus aF
        );
    X.cache#(abstractVariety, B)
    )

beginDocumentation()

doc ///
Key
  AbstractToricVarieties
Headline
  links abstract simplicial (normal) toric varieties to Schubert2
Description
  Text
     This package is experimental.
///

end--

doc ///
Key
Headline
Usage
Inputs
Outputs
Consequences
Description
  Text
  Example
  Code
  Pre
Caveat
SeeAlso
///

TEST ///
-- test code and assertions here
-- may have as many TEST sections as needed
///

end--
restart
loadPackage "AbstractToricVarieties"
