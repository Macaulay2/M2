needsPackage "Bertini"

R = CC[x,y]
F = {x^2-2,y^2-2}
sols = bertiniZeroDimSolve (F)
S = bertiniRefineSols (F,sols,100)
coords = coordinates S_0
coords_0

<<<<<<< HEAD
     R = CC[x,y]
     F = {x^2-2,y^2-2}
     sols = bertiniZeroDimSolve (F)
     S = bertiniRefineSols (F,sols,100)
     coords = coordinates S_0
     coords_0
     
     end
     restart
     load "Bertini/TST/bertiniRefineSols.tst.m2"
     
=======
end

restart
load "Bertini/TST/bertiniRefineSols.tst.m2"
>>>>>>> fb1f8166c18d97872c79f9732bc4c1e6379b6156
