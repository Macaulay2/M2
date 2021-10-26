needsPackage "Bertini"

--   Example
     writeParameterFile(storeBM2Files,{2,3,5,7})
     assert fileExists(storeBM2Files|"/final_parameters")
     moveB'File(storeBM2Files,"final_parameters","start_parameters")
     assert fileExists(storeBM2Files|"/start_parameters")
     moveB'File(storeBM2Files,"start_parameters","backup",CopyB'File=>true)
     assert fileExists(storeBM2Files|"/start_parameters")
     assert fileExists(storeBM2Files|"/backup")
--   Example
     Dir1 = temporaryFileName()
     makeDirectory Dir1
     writeParameterFile(storeBM2Files,{2,3,5,7})
     moveB'File(storeBM2Files,"final_parameters","start_parameters",MoveToDirectory=>Dir1)
     assert fileExists(Dir1|"/start_parameters")
--   Example
     makeDirectory (storeBM2Files|"/Dir2")
     writeParameterFile(storeBM2Files,{2,3,5,7})
     moveB'File(storeBM2Files,"final_parameters","start_parameters",SubFolder=>"Dir2")
     assert fileExists(storeBM2Files|"/Dir2/start_parameters")
     