needsPackage "Bertini"

--To solve a zero dimensional system use the bertiniZeroDimSolve command
-- the input is a list of equations 
-- the outpt is a list of zero dimensional solutions
R=QQ[x,y,z]

--this input is a square system
outputSquareSystem=bertiniZeroDimSolve({"(z)^2*(x^2-9)",y-2,z-1},AllowStrings=>{x,y,z})
assert(areEqual(outputSquareSystem, {point {{3, 2, 1}}, point {{-3, 2, 1}}}))

--the input system may be overdetermined
outputOverdetermined=bertiniZeroDimSolve({(z)^2*(x^2-9),y-2,z-1,x-3})
assert(areEqual(outputOverdetermined, {point {{3,2,1}}}))

