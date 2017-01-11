needsPackage "Bertini"

R=QQ[x,y]**QQ[a1,a2]
f=x^3+x*y^2+y+y^3+x-2
h=a1*x+a2*y-1
--f defines a cubic surface. The intersection with the hyerplane h is 3 points. 
--We consider f=h=0 as a parameterized system with parameters a1,a2 and unknowns x,y.

--The parameters (a1,a2)=(1,0) has a solution (x,y)=(1,0).
sub(f,{x=>1,y=>0,a1=>1,a2=>0})==0
sub(h,{x=>1,y=>0,a1=>1,a2=>0})==0

--we write a start file:
writeStartFile(storeBM2Files,{{1,0}},NameStartFile=>"startSave")
assert(fileExists(storeBM2Files|"/startSave"))
--write a start_parameter file. Note that you need to name the parameter file as "start_parameters" because the default is "final_parameters"
writeParameterFile(storeBM2Files,{1,0},NameParameterFile=>"start_parameters")
assert(fileExists(storeBM2Files|"/start_parameters"))
--Now we write our Bertini input file with PARAMETERHOMOTOPY set to 2. 

f=toString f
h=toString h
makeB'InputFile(storeBM2Files,
    B'Configs=>{
	{PARAMETERHOMOTOPY,2},
	{MPTYPE,2}},
    AffVariableGroup=>{x,y},
    ParameterGroup=>{a1,a2},
    B'Polynomials=>{f,h}    )
collectedSols=b'PHMonodromyCollect(storeBM2Files,
    NameStartFile=>"startSave",
    NameSolutionsFile=>"simple_raw_solutions",
      	NumberOfLoops=>10,NumSolBound=>3,
	MonodromyStartParameters=>{1,0}
	)
assert(#collectedSols>0)
assert(abs(collectedSols_0_0-1)<1e-6)
traceY=sum for i in collectedSols list i_1
if #collectedSols==3 then assert(abs(-1-traceY)<1e-6)



---
     f="x^3+x*y^2+y+y^3+x-2";     h="a1*x+a2*y-1";
     makeB'InputFile(storeBM2Files, 
    	 B'Configs=>{{PARAMETERHOMOTOPY,2},{MPTYPE,2}},AffVariableGroup=>{x,y},ParameterGroup=>{a1,a2}, B'Polynomials=>{f,h}    )
     b'PHMonodromyCollect(storeBM2Files,
	 StorageFolder=>"StoreFiles",
	 MonodromyStartPoints=>{{1,0}},
      	 NumberOfLoops=>10,NumSolBound=>3,
	 MonodromyStartParameters=>{1,0}	)
assert(#importSolutionsFile(storeBM2Files,StorageFolder=>"StoreFiles")>0)



