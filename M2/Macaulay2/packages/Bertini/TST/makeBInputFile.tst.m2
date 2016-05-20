needsPackage "Bertini"

--To solve a positive dimensional system use the bertiniPosDimSolve command
-- the input is a list of equations 
-- the outpt is a numerical variety
makeB'InputFile(storeBM2Files)
assert ( fileExists(storeBM2Files|"/input")===true)



