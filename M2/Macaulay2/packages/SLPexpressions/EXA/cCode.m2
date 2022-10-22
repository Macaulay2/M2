restart
debug needsPackage "NumericalAlgebraicGeometry"
NumericalAlgebraicGeometry#"exported symbols"
R = CC[x,y,z]
F = for i to 2 list random(2,R)-1 
(G,solsG) = totalDegreeStartSystem F
H = segmentHomotopy(F,G)
keys H

inputs = H#"X" | matrix{{H#"T"}}
numColumns inputs
printAsSLP H#"H"

cCode (H#"H", inputs)
cCode (H#"Ht", inputs)
cCode (H#"Hx", inputs)
cCode (H#"Hx"|H#"Ht", inputs)

