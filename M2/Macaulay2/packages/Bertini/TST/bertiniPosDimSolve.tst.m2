needsPackage "Bertini"

--To solve a positive dimensional system use the bertiniPosDimSolve command
-- the input is a list of equations 
-- the outpt is a numerical variety
R=QQ[x,y,z]
outHyper=bertiniPosDimSolve(
     {(z+2)*x*(x^2+y^2-1),(y-1)*(2+y)*(x^2+y^2-1),(2+y)*(x-3)*(x^2+y^2-1)})

numDecomp=for i from 0 to 2 list  apply(outHyper#i,degree)
assert (numDecomp=={{1}, {1, 1}, {2}})

