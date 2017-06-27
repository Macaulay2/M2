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
        PackageExports => {"NormalToricVarieties", "Schubert2"},
        DebuggingMode => true
        )

export {    
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

  abstractVariety(NormalToricVariety,AbstractVariety) := opts -> (Y,B) -> (
      if not isSimplicial Y then error "abstract variety for non-simplicial toric varieties not yet implemented";
      if not Y.cache#?(abstractVariety, B) then Y.cache#(abstractVariety,B) = (
          kk := intersectionRing B;
          IY := intersectionRing Y;
          amb := kk[gens ambient IY, Join=>false];
          IY = amb/(sub(ideal IY, vars amb));
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

end--
doc ///
Key
  AbstractToricVarieties
Headline
Description
  Text
  Example
Caveat
SeeAlso
///

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
