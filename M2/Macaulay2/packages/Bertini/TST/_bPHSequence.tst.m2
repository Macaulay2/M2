needsPackage "Bertini"

----Test Parameter homotopy and PreparaPH2.

--Parameter homotopy test.
makeB'InputFile(storeBM2Files,
    AffVariableGroup=>{x},
    ParameterGroup=>{u},
    BertiniInputConfiguration=>{{PARAMETERHOMOTOPY,1}},
    B'Polynomials=>{"x^2+u"}
    )
runBertini(storeBM2Files)
---There are two solutions for a general choice of parameters. Bertini makes a random choice of parameters when PARAMETERHOMOTOPY is set to 1.
assert( #importSolutionsFile(storeBM2Files)==2)
assert(#importParameterFile(storeBM2Files,NameParameterFile=>"start_parameters")==1)
---now to do starge two of the parameter homotopy we need to write our parameters and set PARAMETERHOMOTOPY to 2.
makeB'InputFile(storeBM2Files,
    AffVariableGroup=>{x},
    ParameterGroup=>{u},
    BertiniInputConfiguration=>{ParameterHomotopy=>2},---NOTE THE 2!
    B'Polynomials=>{"x^2+u"}
    )
writeParameterFile(storeBM2Files,{ -2},NameParameterFile=>"final_parameters")
runBertini(storeBM2Files)

assert(#importSolutionsFile(storeBM2Files)==2)
assert(#importParameterFile(storeBM2Files,NameParameterFile=>"final_parameters")==1)
assert( abs(((importSolutionsFile(storeBM2Files))_0_0)^2-2)<1e-7 )
assert(abs(   (importParameterFile(storeBM2Files,NameParameterFile=>"final_parameters"))_0- -2)<1e-10    )


----To stream line this process we can use the PreparePH2 option of runBertini
makeB'InputFile(storeBM2Files,
    AffVariableGroup=>{x},
    ParameterGroup=>{u},
    BertiniInputConfiguration=>{ParameterHomotopy=>1},
    B'Polynomials=>{"x^4+u"}
    )
runBertini(storeBM2Files,PreparePH2=>true)
writeParameterFile(storeBM2Files,{-4},NameParameterFile=>"final_parameters")
runBertini(storeBM2Files)

importMainDataFile(storeBM2Files)
importSolutionsFile(storeBM2Files)
importSolutionsFile(storeBM2Files,NameSolutionsFile=>"nonsingular_solutions")
importParameterFile(storeBM2Files,NameParameterFile=>"final_parameters")

assert(#importMainDataFile(storeBM2Files)==4)
assert( abs((importMainDataFile(storeBM2Files))_0#Coordinates_0^4-4)<1e-7 )

assert(#importSolutionsFile(storeBM2Files)==4)
assert(#importParameterFile(storeBM2Files,NameParameterFile=>"final_parameters")==1)
assert( abs(((importSolutionsFile(storeBM2Files))_0_0)^4-4)<1e-7 )
assert(abs(   (importParameterFile(storeBM2Files,NameParameterFile=>"final_parameters"))_0- -4)<1e-7    )


--Test bPHSequence
--This is steamlined with the b'PHSequence command. 

       makeB'InputFile(storeBM2Files, ParameterGroup=>{t1,t2},AffVariableGroup=>{{x,y}},
           BertiniInputConfiguration=>{"PARAMETERHOMOTOPY"=>1},
           B'Polynomials=>{"x^3-t1","y-t2"})
       runBertini(storeBM2Files,PreparePH2=>true)
       setsOfParameters={{1,1},{1,2}}
       b'PHSequence(storeBM2Files, setsOfParameters, SaveData=>true)

--When we have the SaveData option set to true we can see the intermediate solutions.
assert(#importSolutionsFile(storeBM2Files)==3)

assert(#importParameterFile(storeBM2Files,NameParameterFile=>"start_parameters1")==2)
assert(#importSolutionsFile(storeBM2Files,NameSolutionsFile=>"nonsingular_solutions1")==3)


assert(#importParameterFile(storeBM2Files,NameParameterFile=>"start_parameters2")==2)
assert(#importSolutionsFile(storeBM2Files,NameSolutionsFile=>"nonsingular_solutions2")==3)





---
     makeB'InputFile(storeBM2Files, ParameterGroup=>{t1,t2},AffVariableGroup=>{{x,y}},	 
	 B'Configs=>{{"PARAMETERHOMOTOPY",1}},
	 B'Polynomials=>{"x^2-t1","y-t2"})
     runBertini(storeBM2Files,PreparePH2=>true)
     b'PHSequence(storeBM2Files,{{1,1},{1,2}},SaveData=>true,StorageFolder=>"StoreMyFiles")

assert(#importParameterFile(storeBM2Files,StorageFolder=>"StoreMyFiles",NameParameterFile=>"start_parameters1")==2)
assert(#importSolutionsFile(storeBM2Files,StorageFolder=>"StoreMyFiles",NameSolutionsFile=>"nonsingular_solutions1")==2)
assert(#importParameterFile(storeBM2Files,StorageFolder=>"StoreMyFiles",NameParameterFile=>"start_parameters2")==2)
assert(#importSolutionsFile(storeBM2Files,StorageFolder=>"StoreMyFiles",NameSolutionsFile=>"nonsingular_solutions2")==2)
