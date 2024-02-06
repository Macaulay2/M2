loadPackage("Bertini",Reload=>true)


---Given a set of sample solutions or a solutions file AND a witness data file (this is created after performing a Bertini positive dimensional run) we can do a component membership test.
--A component membership test is tracktype 3.

    makeB'InputFile(storeBM2Files,
    	BertiniInputConfiguration=>{{TrackType,1}},    AffVariableGroup=>{x,y,z},
	B'Polynomials=>{"z*((x+y+z)^3-1)","z*(y^2-3+z)"}    )
---By running Bertini we will create a witness_data file.
    runBertini(storeBM2Files)
assert(7==#importMainDataFile(storeBM2Files))
--We create solutions file.
    makeSampleSolutionsFile(storeBM2Files,2,SpecifyComponent=>{1,0},NameSolutionsFile=>"our_test_solutions")
--Now we do a membership test which creates an incidenceMatrix file.
assert(fileExists(storeBM2Files|"/our_test_solutions"))
    makeMembershipFile(storeBM2Files,
	NameSolutionsFile=>"our_test_solutions")

--Import the information of the membership test we need to import the incidence matrix.
theIM=    importIncidenceMatrix(storeBM2Files)
assert(2==#theIM)
assert({(2,0)}==theIM_0)
assert((2,0)==theIM_0_0)

--Our incidence matrix is flattened to a list.
--The number of elements in theIM is equal to the number of points in the solutions file.
--Each element of theIM is a list of sequences of 2 elements (codim,component Number).
--Note that we follow the Bertini convention and switch from (dimension,component number) indexing to (codimension,component number) indexing.

    makeMembershipFile(storeBM2Files,
	TestSolutions=>{{1,2,0},{3,2,1}})
newIM=    importIncidenceMatrix(storeBM2Files)
assert(newIM_0=={(1,0)})
-- says that the 0th point of TestSolutions is in component (1,0) meaning in codimension 1 the 0th component.
assert(newIM_1=={})
-- says that the other point of TestSolutions is in no components.



---------It is possible for a TestSolution to be in more than one point.
    makeB'InputFile(storeBM2Files,
    	BertiniInputConfiguration=>{{TrackType,1}},    AffVariableGroup=>{x,y,z},
	B'Polynomials=>{"x*y"}    )
---By running Bertini we will create a witness_data file.
    runBertini(storeBM2Files)
ourTestSolutions={{0,0,1},{0,1,1},{1,0,1},{1,1,1}}
    makeMembershipFile(storeBM2Files,
	TestSolutions=>ourTestSolutions)
anotherIM=importIncidenceMatrix(storeBM2Files)
assert(2==#anotherIM_0)--Says ourTestSolutions_0 is in two components
assert(1==#anotherIM_1)--Says ourTestSolutions_1 is in one component
assert(1==#anotherIM_2)--Says ourTestSolutions_2 is in one component
assert(0==#anotherIM_3)--Says ourTestSolutions_3 is in no components.
