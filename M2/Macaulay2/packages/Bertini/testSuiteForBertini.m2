help   "bertiniZeroDimSolve"--check
help  "bertiniParameterHomotopy",---example in summer Demo just need todocument
help  "bertiniPosDimSolve" --check
help  "bertiniSample"--check
help  "bertiniTrackHomotopy"
--help  "bertiniSolve"
help   "bertiniComponentMemberTest"--something seems off
help  "bertiniRefineSols"--check



---REMOVE FLAG
--This file demonstrates the progess we have made on the bertini package thus far.
-- Always load this file first so bertini.m2 is loaded from the correct place. 
----This is unique to Jose's MacMath computer
restart
path=prepend("/Applications/Macaulay2-1.5/gitStuff/M2/M2/Macaulay2/packages",path)
install
loadPackage("Bertini")
peek Bertini


---The variables of your ring consist of parameters, then unknowns.
R=QQ[t,x,y]
--We have a system of two equations.
aUserHomotopy={(t)^2*(x^2-1)+(1-t)^2*(x^2-9),y-1}
Z--the start points for the homotopy are for t=1 and below:
startPoints={point({{1,1}}),
     point({{-1,1}})}

--the input for bertiniTrackHomotopy is 
---1) start system
---2) parameter
---3) start points

--the output for bertiniTrackHomotopy is a list of points. 
targetPoints=bertiniTrackHomotopy(aUserHomotopy,t,startPoints)




restart
path=prepend("/Applications/Macaulay2-1.5/gitStuff/M2/M2/Macaulay2/packages",path)
installPackage("Bertini")
--To solve a zero dimensional system use the bertiniZeroDimSolve command
-- the input is a list of equations 
-- the outpt is a list of zero dimensional solutions
R=QQ[x,y,z]

--this input is a square system
outputSquareSystem=bertiniZeroDimSolve({(z)^2*(x^2-9),y-2,z-1})

--the input system may be overdetermined
outputOverdetermined=bertiniZeroDimSolve({(z)^2*(x^2-9),y-2,z-1,x-3})

--the input system CANNOT be underdetermined and return an output
outputUnderdetermined=bertiniZeroDimSolve({(1-z)^2*(x^2-9),y-2})

--When we have solutions with multiplicity they are recorded twice in our list
outputMultiplicity=bertiniZeroDimSolve({(z)^2*(x^2-9),y-2,(z-1)^2})
peek oo_0

--You can compare the condition numbers of points to these solutions
peek outputSquareSystem_0
peek outputOverdetermined_0---whydoes this say solution number -1???
peek outputMultiplicity_0



--restart
--Another example of a zero dimensional solve
R=QQ[x,y]
f1=(x-1)^4*(x+2)
f2=(y-2)^3
soks=bertiniZeroDimSolve({f1,f2})
peek soks_0

--
restart
path=prepend("/Applications/Macaulay2-1.5/gitStuff/M2/M2/Macaulay2/packages",path)
installPackage("Bertini")

---This is an example of bertiniPosDimSolve
R=QQ[x,y]
f1=x^3-1
f2=y^3-1
--input a system outputs a numerical variety
Aba=bertiniPosDimSolve(
     {x^3-1,y^3-1})
peek Aba#0_0

Aba2=bertiniPosDimSolve(
     {f1})
peek Aba2#1_0

---This is an example of bertiniPosDimSolve with multiplicity
R=QQ[x,y]
f1=(x^2-1)^2

posSolveWithMult=bertiniPosDimSolve(
     {f1})
peek oo
sampleSolveWithMult=bertiniSample(
     {f1},dimen=>1,compnum=>1,numpts=>12,WitnessData=>"/tmp/M2-2361-0/5/witness_data")
--the degree is not correct if we are counting multiplicities
--Do we want numerical varieties to carry scheme structure?


oo#1
keys o20
o21_0
peek oo

----refined solutions
R=QQ[x,y]
f1=x^3-1
f2=y^3-1
L={f1, f2}
pts=bertiniZeroDimSolve(L)
rdf="/Users/MacMath/Desktop/tmp/M2-20172-0/5/raw_data" 
bertiniRefineSols(L,RawData=>rdf,StartSolutions=>{{1,
-.5+.866025*ii}},digits=>100)




---Get sample to work
---This is an example of bertiniSample
--We do a pos run to get witness_data file found 

R=QQ[x,y]
f1=x^3-1
f2=y^3-1
wdf="/Users/MacMath/Desktop/SampleExample/witness_data"


bertiniSample(
     {f1},dimen=>1,compnum=>1,numpts=>12,WitnessData=>wdf)


--parameter
restart
path=prepend("/Applications/Macaulay2-1.5/gitStuff/M2/M2/Macaulay2/packages",path)
installPackage("Bertini")

--To have a parameter homotopy you need to create a ring of unknowns and aparemters
--the input is 
--- 1) a list of polynomials that make your start system,
--- 2) a list of the parameters, 
--- 3) a list of final parameters (this is a list of lists of complex numbers)


S=QQ[u1,u2,u3,x,y]
f1=u1*(y-1)+u2*(y-2)+u3*(y-3)
f2=(x-11)*(x-12)*(x-13)
bPH=bertiniParameterHomotopy({f1,f2},{u1,u2,u3},{
	  {{1,0,0}},
	  {{0,1,0}}
	  })
netList bPH



