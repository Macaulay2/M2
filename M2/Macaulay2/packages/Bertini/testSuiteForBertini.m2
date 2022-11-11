help   "bertiniZeroDimSolve"--check
help  "bertiniParameterHomotopy",---example in summer Demo just need todocument
help  "bertiniPosDimSolve" --check
help  "bertiniSample"--check
help  "bertiniTrackHomotopy"
--help  "bertiniSolve"
help   "bertiniComponentMemberTest"--something seems off
help  "bertiniRefineSols"--check



-- <<<<<<< HEAD
--
restart
path=prepend("/Applications/Macaulay2-1.5/gitStuff/M2/M2/Macaulay2/packages",path)
loadPackage("Bertini")

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
-- =======
-- >>>>>>> f6913c93582189b936de324a20911184ac79b04c

----refined solutions--broken
R=CC[x,y]
f1=x^3-1
f2=y^3-1
F={f1, f2}
pts=bertiniZeroDimSolve(F)
pts_0
rdf="/Users/MacMath/Desktop/tmp/M2-20172-0/25/" 
bertiniRefineSols(F,pts,100)
bertiniRefineSols(F,{{1,0},{1,0}},100)
help bertiniRefineSols

 R = CC[x,y]
     F = {x^2-2,y^2-2}
     sols = bertiniZeroDimSolve (F)
     S = bertiniRefineSols (F,sols,100)



---Get sample to work
---This is an example of bertiniSample
--We do a pos run to get witness_data file found 

R=QQ[x,y]
f1=x^3-1
f2=y^3-1
wdf="/Users/MacMath/Desktop/SampleExample/witness_data"


bertiniSample(
     {f1},dimen=>1,compnum=>1,numpts=>12,WitnessData=>wdf)




help bertiniSample
--parameter
restart
path=prepend("/Applications/Macaulay2-1.5/gitStuff/M2/M2/Macaulay2/packages",path)
loadPackage("Bertini")

--To have a parameter homotopy you need to create a ring of unknowns and parameters
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

R=QQ[x,y,z]
bertiniPosDimSolve({x^2-1},FINALTOL=>1e-6)
bertiniPosDimSolve({x^2-1},FINALTOL=>1e-10)
NV = bertiniPosDimSolve({x^2-1},FINALTOL=>1e-16, PRECISION=>128, MPTYPE=>0)
first first points NV#2#0
