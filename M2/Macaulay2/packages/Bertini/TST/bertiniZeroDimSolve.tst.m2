loadPackage("Bertini",Reload=>true)

--To solve a zero dimensional system use the bertiniZeroDimSolve command
-- the input is a list of equations 
-- the outpt is a list of zero dimensional solutions
R=QQ[x,y,z]

--this input is a square system
outputSquareSystem=bertiniZeroDimSolve({(z)^2*(x^2-9),y-2,z-1})
assert(areEqual(sortSolutions outputSquareSystem, {point {{-3, 2, 1}},point {{3, 2, 1}}}))

--the input system may be overdetermined
outputOverdetermined=bertiniZeroDimSolve({(z)^2*(x^2-9),y-2,z-1,x-3})
assert(areEqual(outputOverdetermined, {point {{3,2,1}}}))

--When we have solutions with multiplicity they are recorded twice in our list (no longer need this test)
--outputMultiplicity=solutionsWithMultiplicity bertiniZeroDimSolve({(z)^2*(x^2-9),y-2,(z-1)^2})
--assert((outputMultiplicity#0).Multiplicity==2 and (outputMultiplicity#1).Multiplicity==2)

R=QQ[x,y]
f1=(x-1)^4*(x+2)
f2=(y-2)^3
soks=bertiniZeroDimSolve({f1,f2})

---
R=CC[u1,u2,u3,x,y]
f1=u1*(y-1)+u2*(y-2)+u3*(y-3)
f2=(x-11)*(x-12)*(x-13)
S1=bertiniZeroDimSolve(
     {f1,f2},--list of polynomials that have zero dimensional solutions
     RandomComplex=>{u1,u2,u3},AffVariableGroup=>{x,y})
assert(#S1===3)
S2=bertiniZeroDimSolve(
     {f1,f2},--list of polynomials that have zero dimensional solutions
     RandomReal=>{u1,u2,u3},AffVariableGroup=>{x,y})
assert(#S2===3)
assert(abs imaginaryPart(S2_0#Coordinates_1)<1e-8)

----------

------
     S3=bertiniZeroDimSolve( {"x^2-u1"}, B'Constants=>{{u1,1}},
	 AffVariableGroup=>{x})            
      assert(1===#radicalList({ (((S3_0)#Coordinates)_0)^2,1})     )
      assert(1===#radicalList({ (((S3_1)#Coordinates)_0)^2,1})     )
---
     S4=bertiniZeroDimSolve( {"x^2-u1*y^2"}, B'Constants=>{{u1,1}},
	 HomVariableGroup=>{x,y})            
    assert(#S4==2)
     S5=bertiniZeroDimSolve( {"x^2-u1*y^2"}, B'Constants=>{{u1,1}},
	 HomVariableGroup=>{x,y},OutputStyle=>"OutSolutions")            
    assert(#S5==2)
    assert(#(S5_0)==2)
    
------
     S6=bertiniZeroDimSolve( {"x^2-1"},
	 AffVariableGroup=>{x},OutputStyle=>"OutSolutions")            
     assert(class S6_0===List)
      assert(1===#radicalList({ (((S6_0))_0)^2,1})     )
      assert(1===#radicalList({ (((S6_1))_0)^2,1})     )
--
     dir1 = temporaryFileName(); -- build a directory to store temporary data 
     makeDirectory dir1;  
     S7=bertiniZeroDimSolve( {"x^2-2"},
	 AffVariableGroup=>{x},OutputStyle=>"OutNone",TopDirectory=>dir1)            
     B0=importSolutionsFile(dir1,NameSolutionsFile=>"raw_solutions")     
     B1=importSolutionsFile(dir1)     
     B2=importMainDataFile(dir1)     
      assert(1===#radicalList({ (B0_0_0)^2,2})     )
      assert(1===#radicalList({ (B1_0_0)^2,2})     )
      assert(1===#radicalList({ ((B2_0#Coordinates)_0)^2,2})     )

---
     dir1 = temporaryFileName(); -- build a directory to store temporary data 
     makeDirectory dir1;  
     S8=bertiniZeroDimSolve( {"x^2-2"},
	 AffVariableGroup=>{x},
	 BertiniInputConfiguration=>{UseRegeneration=>1,MPType=>2},
	 OutputStyle=>"OutNone",TopDirectory=>dir1)            
     B0=importSolutionsFile(dir1,NameSolutionsFile=>"raw_solutions")     
     B1=importSolutionsFile(dir1)     
     B2=importMainDataFile(dir1)     
--TO DO: need assertions
---
     dir1 := temporaryFileName(); -- build a directory to store temporary data 
     makeDirectory dir1;  
     S9=bertiniZeroDimSolve( {"(x-2)^2"},
	 AffVariableGroup=>{x},
	 BertiniInputConfiguration=>{MPType=>2},
	 OutputStyle=>"OutNone",TopDirectory=>dir1)            
     B0=importSolutionsFile(dir1,NameSolutionsFile=>"raw_solutions")     
     B1=importSolutionsFile(dir1)     
     B2=importMainDataFile(dir1)     
--TODO: Need assertions
-*
      assert(1===#radicalList({ (B0_0_0),2})     )
      assert(1===#radicalList({ (B1_0_0),2})     )
      assert(1===#radicalList({ ((B2_0#Coordinates)_0),2})     )
      assert(1===#radicalList(flatten B0))
      assert( B2_0#Multiplicity===2)
------Multi Affine variable groups
     dir1 := temporaryFileName(); -- build a directory to store temporary data 
     makeDirectory dir1;  
     S10=bertiniZeroDimSolve( {"(x-2)^2","y-4"},
	 AffVariableGroup=>{{x},{y}},
	 B'Configs=>{MPType=>2},
	 OutputStyle=>"OutNone",TopDirectory=>dir1)            
     B0=importSolutionsFile(dir1,NameSolutionsFile=>"raw_solutions")     
     B1=importSolutionsFile(dir1)     
     B2=importMainDataFile(dir1)     
      assert(1===#radicalList({ (B0_0_0),2})     )
      assert(1===#radicalList({ (B1_0_0),2})     )
      assert(1===#radicalList({ ((B2_0#Coordinates)_0),2})     )
      assert(1===#radicalList(flatten (B0/first)))
      assert( B2_0#Multiplicity===2)
    *-
      
    

---test M2Precision
-*
s1= bertiniZeroDimSolve({"(x^2-3+ii)"},B'Configs=>{{FinalTol,1e-100}},UseRegeneration=>1,AffVariableGroup=>{x},M2Precision=>300,OutputStyle=>"OutSolutions")
s2= bertiniZeroDimSolve({"(x^2-3+ii)"},B'Configs=>{{FinalTol,1e-100}},UseRegeneration=>1,AffVariableGroup=>{x},M2Precision=>300)
s3= bertiniZeroDimSolve({"(x^2-3+ii)"},B'Configs=>{{FinalTol,1e-100}},UseRegeneration=>1,AffVariableGroup=>{x},OutputStyle=>"OutSolutions")
s4= bertiniZeroDimSolve({"(x^2-3+ii)"},B'Configs=>{{FinalTol,1e-100}},UseRegeneration=>1,AffVariableGroup=>{x})

assert(#toExternalString(s1_0_0)>190)
assert(#toExternalString(s3_0_0)<190)
assert(#toExternalString((coordinates (s2_0))_0)>190)
assert(#toExternalString((coordinates (s4_0))_0)<190)
*-
