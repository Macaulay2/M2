needsPackage "Bertini"

makeB'InputFile(storeBM2Files,AffVariableGroup=>{x},B'Polynomials=>{"x-1"});
runBertini(storeBM2Files);
assert(fileExists(storeBM2Files|"/"|"bertini_session.log")===true)
assert(fileExists(storeBM2Files|"/"|"nonsingular_solutions")===true)



