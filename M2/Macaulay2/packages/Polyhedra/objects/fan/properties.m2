compute#Fan#smooth = method()
compute#Fan#smooth Fan := F -> (
   R := rays F;
   L := transpose linealitySpace F;
   MC := maxCones F;
   MC = apply(MC,
      m -> R_m
   );
   all(MC, r -> spanSmoothCone(transpose r, L))
)

compute#Fan#computedDimension = method()
compute#Fan#computedDimension Fan := F -> (
   R := rays F;
   MC := maxCones F;
   L := linealitySpace F;
   MC = apply(MC, m -> R_m);
   MC = apply(MC, r -> rank (r | L));
   max MC
)

compute#Fan#honestMaxObjects = method()
compute#Fan#honestMaxObjects Fan := F -> (
   R := rays F;
   MC := maxCones F;
   L := linealitySpace F;
   apply(MC, m -> posHull(R_m, L))
)

compute#Fan#computedComplete = method()
compute#Fan#computedComplete Fan := F -> (
   n := dim F;
   if n != ambDim F then return false;
   symmDiff := (X,Y) -> (
      summand1 := select(X, x -> position(Y, y->y==x) === null); 
      summand2 := select(Y, y -> position(X, x->y==x) === null); 
      flatten {summand1, summand2}
   );
   MC := getProperty(F, honestMaxObjects);
   Lfaces := {};
   CFsave := {};
   scan(MC, 
      C -> (
         if dim C == n then (
            R := rays C;
            L := linealitySpace C;
            CFacets := toList getProperty(C, computedFacetsThroughRays);
            CFacets = apply(CFacets, facet -> posHull(R_facet, L));
            CFsave = flatten {CFsave, {CFacets}};
            Lfaces = symmDiff(Lfaces, CFacets);
         )
         else return false
      )
   );
   Lfaces == {}
)
