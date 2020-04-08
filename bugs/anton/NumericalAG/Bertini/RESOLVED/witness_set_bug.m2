needsPackage "Bertini"
R = CC[x,y]
F = {x*y}
V = bertiniPosDimSolve F
c = first components V
evaluate(polySystem slice c, first c.Points) -- should be ~ 0
