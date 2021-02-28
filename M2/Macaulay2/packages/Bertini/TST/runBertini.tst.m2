needsPackage "Bertini"

makeB'InputFile(storeBM2Files,AffVariableGroup=>{x},B'Polynomials=>{"x-1"});
runBertini(storeBM2Files);
assert(fileExists(storeBM2Files|"/"|"bertini_session.log")===true)
assert(fileExists(storeBM2Files|"/"|"nonsingular_solutions")===true)
----------------------------------------
makeB'InputFile(storeBM2Files,
    AffVariableGroup=>{x,y},
    B'Polynomials=>{"x^2-1","y^2-4"})
runBertini(storeBM2Files,StorageFolder=>"StoreMyFiles")
assert(fileExists(storeBM2Files|"/StoreMyFiles/"|"bertini_session.log")===true)
assert(fileExists(storeBM2Files|"/StoreMyFiles/"|"nonsingular_solutions")===true)


