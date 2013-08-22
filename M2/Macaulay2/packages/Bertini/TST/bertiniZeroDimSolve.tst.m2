needsPackage "Bertini"

--To solve a zero dimensional system use the bertiniZeroDimSolve command
-- the input is a list of equations 
-- the outpt is a list of zero dimensional solutions
R=QQ[x,y,z]

--this input is a square system
outputSquareSystem=bertiniZeroDimSolve({(z)^2*(x^2-9),y-2,z-1})
assert(areEqual(outputSquareSystem, {point {{3, 2, 1}}, point {{-3, 2, 1}}}))

--the input system may be overdetermined
outputOverdetermined=bertiniZeroDimSolve({(z)^2*(x^2-9),y-2,z-1,x-3})
assert(areEqual(outputOverdetermined, {point {{3,2,1}}}))

--the input system CANNOT be underdetermined and return an output
/// -- can we make it fail gracefully?
outputUnderdetermined=bertiniZeroDimSolve({(1-z)^2*(x^2-9),y-2})
///

--When we have solutions with multiplicity they are recorded twice in our list
outputMultiplicity=solutionsWithMultiplicity bertiniZeroDimSolve({(z)^2*(x^2-9),y-2,(z-1)^2})
assert((outputMultiplicity#0).Multiplicity==2 and (outputMultiplicity#1).Multiplicity==2)

R=QQ[x,y]
f1=(x-1)^4*(x+2)
f2=(y-2)^3
soks=bertiniZeroDimSolve({f1,f2})
