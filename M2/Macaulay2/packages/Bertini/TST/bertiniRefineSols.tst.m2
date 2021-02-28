restart
loadPackage("Bertini",Reload=>true)
printingPrecision =600
R = CC[x,y]
F = {x^2-2,y^2-2}
sols = bertiniZeroDimSolve (F)
S = bertiniRefineSols (200, F, sols)
coords = flatten entries matrix S_0
--TODO: to get the correct assertion one need to adjust have printingPrecision =300
--TODO need to be clear on how much precision or digits we are using.
assert( abs(abs(coords_0) - sqrt(2p333)) < 1e-100)

end


