      R = CC[x,y];
      F = {x^2-2,y^2-2};
      W = bertiniZeroDimSolve (F)
      S = bertiniRefineSols (300,W)
      coords = coordinates S_0
      coords_0
      assert( abs( (coords_0)^2 - 2) < 1e-100)


