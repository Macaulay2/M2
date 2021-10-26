loadPackage("Bertini",Reload=>true)
check"Bertini"

---After doing a positive dimensional run we can sample points from components. 

--First we do a positive dimensional run.
makeB'InputFile(storeBM2Files,
     AffVariableGroup=>{x,y,z},
     BertiniInputConfiguration=>{TrackType=>1},
     B'Polynomials=>{"(x^2+y^2+z^2-1)*y"})  
runBertini(storeBM2Files)
assert(#importMainDataFile(storeBM2Files)==3)

--Next we will make a solutions file using the makeSampleSolutionsFile. 
--This command will rewrite the the tracktype in the input file to tracktype 2, then call Bertini to perform a sample. 

makeSampleSolutionsFile(storeBM2Files,5,SpecifyComponent=>{2,0},
    	NameSolutionsFile=>"sample_solutions_file2" --this is the default.
	)
assert(5==#importSolutionsFile(storeBM2Files,NameSolutionsFile=>"sample_solutions_file2"))

makeSampleSolutionsFile(storeBM2Files,6,SpecifyComponent=>{2,0}	)
assert(6==#importSolutionsFile(storeBM2Files,NameSolutionsFile=>"sample_solutions_file")    )
----------------------------------------------


---Again first do a positive dimensional run, and then we can sample a component that contains a particular witness point. 
makeB'InputFile(storeBM2Files,
     AffVariableGroup=>{x,y,z},
     B'Configs=>{{TrackType,1}},
     B'Polynomials=>{"(x^2+y^2+z^2-1)*y"})  
runBertini(storeBM2Files)
assert(#importMainDataFile(storeBM2Files)==3)

onePoint=(importMainDataFile(storeBM2Files))_0
assert(class onePoint===Point)
makeSampleSolutionsFile(storeBM2Files,7,
    SpecifyComponent=>onePoint	)---We sample from the component containing onePoint.
assert(7==#importSolutionsFile(storeBM2Files,NameSolutionsFile=>"sample_solutions_file"))

