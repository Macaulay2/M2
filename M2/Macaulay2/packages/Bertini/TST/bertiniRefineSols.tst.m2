
needsPackage"Bertini"

     R = CC[x,y]
     F = {x^2-2,y^2-2}
     sols = bertiniZeroDimSolve (F)
     S = bertiniRefineSols (F,sols,100)
     coords = coordinates S_0
     coords_0