needsPackage "Bertini"

--To solve a positive dimensional system use the bertiniPosDimSolve command
-- the input is a list of equations 
-- the outpt is a numerical variety
R=QQ[x,y,z]
f="x+y+z"
g=x^2-y^2

output0=bertiniZeroDimSolve({f,g},ISPROJECTIVE=>1,AllowStrings=>gens R)
assert ( #coordinates output0_0==3)

output1=bertiniPosDimSolve({f,g},ISPROJECTIVE=>1,AllowStrings=>gens R)
assert (0==dim (output1#0_0))

bertiniParameterHomotopy({f,g},{u},{{1},{4}},ISPROJECTIVE=>1,AllowStrings=>gens R)

S=QQ[x,y,z,u1,u2]
h1=(u1+u2)*x-(u1+u2)*y+z
h2=x^2-y^2
outputPH=bertiniParameterHomotopy({h1,h2},{u1,u2},{ {1,4},{2,3},{5,-1}},ISPROJECTIVE=>1,AllowStrings=>{x,y,z})
assert (3==#outputPH)
--assert (sum (coordinates outputPH_0_0)<10^-16) -- this does not work all the time!!!


