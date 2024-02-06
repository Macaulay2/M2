
needsPackage("Bertini")
--To solve a positive dimensional system use the bertiniPosDimSolve command
-- the input is a list of equations 
-- the outpt is a numerical variety
R=QQ[x,y,z]
f=x+y+z
g=x^2-y^2

output0=bertiniZeroDimSolve({f,g},IsProjective=>1)
assert ( #coordinates output0_0==3)
peek output0#0
gens R
--output1=bertiniPosDimSolve({f,g},IsProjective=>1)
--assert (0==dim (output1#0_0))


