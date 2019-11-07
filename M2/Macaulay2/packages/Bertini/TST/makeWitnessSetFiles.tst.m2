needsPackage "Bertini"


---Consider a TrackType 1 run. This run yields a main_data_file where we read in information about the witness points when we use the importMainDataFile command.
---However, we do not import the slice information. 
---To get slice information we need to create witness set files by doing a track type 4.
---The witness set file varies by dimension. Our input is a directory along with a dimension. 
makeB'InputFile(storeBM2Files,
     AffVariableGroup=>{x,y,z},
     BertiniInputConfiguration=>{TrackType=>1},
     B'Polynomials=>{"(x^2+y^2+z^2-1)*y"})  
runBertini(storeBM2Files)
makeWitnessSetFiles(storeBM2Files,2)--creats a witness point file for all dimension 2 components and a linear slice file for dimension 2 components. 
--When we import the SliceFile, we get a list.
----The first element of the list is a list of pairs of coefficient names and their value.
----The second element of the list is a list of pairs of names of linear functions and the lieanr function. 
L=importSliceFile(storeBM2Files) 
assert(#L==2)
assert (#L_0==8)
assert (#L_1==2)

---The makeWitnessSetFiles command also creates a witness point file along with the SliceFile. 
--The default name of the witness point file is "witness_solutions_file"
--This default can be changed using the NameWitnessPointFile option.
assert(#importSolutionsFile(storeBM2Files,NameSolutionsFile=>"witness_solutions_file")==3)


makeWitnessSetFiles(storeBM2Files,2,
    NameSolutionsFile=>"new_name_points",--creates a witness point file with a custom name. 
    SpecifyComponent=>0)  --Component indexing begins at 0. The function creates a witness point file for only a particular component. 

s=importSolutionsFile(storeBM2Files,NameSolutionsFile=>"new_name_points")
assert(#s==1 or #s==2)

--
     storeBM2Files2 = temporaryFileName()
     makeDirectory storeBM2Files2
makeB'InputFile(storeBM2Files2,
     AffVariableGroup=>{x,y,z},
     BertiniInputConfiguration=>{TrackType=>1},
     B'Polynomials=>{"(x^2+y^2+z^2-1)*y"})  
 runBertini(storeBM2Files2)
     makeWitnessSetFiles(storeBM2Files2,2,
       NameSolutionsFile=>"custom_name_witness_points",--creates a witness point file with a custom name. 
       SpecifyComponent=>0)  --Component indexing begins at 0. The function creates a witness point file for only a particular component. 
     L1=importSliceFile(storeBM2Files2) 
     S0=importSolutionsFile(storeBM2Files2,NameSolutionsFile=>"custom_name_witness_points")

assert(1==#S0 or 2==#S0)
--
     makeWitnessSetFiles(storeBM2Files,2,
       NameSolutionsFile=>"custom_name_witness_points")
     S=importSolutionsFile(storeBM2Files,NameSolutionsFile=>"custom_name_witness_points")
assert(3==#S)
-------------------------------------------------------