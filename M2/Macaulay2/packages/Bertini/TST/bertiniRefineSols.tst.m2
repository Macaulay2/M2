needsPackage "Bertini"

R = CC[x,y]
F = {x^2-2,y^2-2}
sols = bertiniZeroDimSolve (F)
S = bertiniRefineSols (100, F, sols)
coords = coordinates S_0
assert( abs(abs(coords_0) - sqrt(2p333)) < 1e-300)
end


