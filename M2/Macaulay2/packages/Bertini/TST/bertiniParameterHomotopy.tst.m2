needsPackage "Bertini"

myPoints={ {
      point{{11, 1}},
       point{{13, 1}},
        point{{12, 1}}	  },
 {
      point{{11, 2}},
       point{{13, 2}},
        point{{12, 2}}	  }}

R=CC[u1,u2,u3,x,y]
f1=u1*(y-1)+u2*(y-2)+u3*(y-3)
f2=(x-11)*(x-12)*(x-13)
bPH=bertiniParameterHomotopy(
     {f1,f2},--list of polynomials that have zero dimensional solutions
     {u1,u2,u3},--your parameters
     {	  {1,0,0},	  {0,1,0}	  })---list of {list of numbers})
apply(bPH/first,{1,2},(P,yValue)->assert(abs((matrix P)_(0,1)-yValue)<1e-6 ))

------
     R=CC[x,u1]
     f1=x^2-u1
     finalParameters0={1}
     finalParameters1={2}
     bPH=bertiniParameterHomotopy( {f1}, {u1},{finalParameters0 ,finalParameters1 },AffVariableGroup=>{x})            
--TODO: need new assertions
--      assert(1===#radicalList({ (((bPH_0_0)#Coordinates)_0)^2,1})     )
--      assert(2===#radicalList({ (((bPH_1_0)#Coordinates)_0)^2,1})     )

     R=CC[x,y,u1]
     f1=x^2-u1*y^2
     finalParameters0={1}
     finalParameters1={2}
     bPH3=bertiniParameterHomotopy( {f1}, {u1},{finalParameters0 ,finalParameters1 },HomVariableGroup=>{x,y})            
    assert(#bPH3==2)
    assert(#bPH3_0==2)
    assert(#bPH3_1==2)
----

------
     R=CC[x,u1]
     f1=x^2-u1
     finalParameters0={1}
     finalParameters1={2}
     bPH4=bertiniParameterHomotopy( {f1}, {u1},{finalParameters0 ,finalParameters1 },AffVariableGroup=>{x},OutputStyle=>"OutSolutions")            
     assert(class bPH4===List)
--TODO: need new assertions
--      assert(1===#radicalList({ (((bPH4_0_0))_0)^2,1})     )
--      assert(2===#radicalList({ (((bPH4_1_0))_0)^2,1})     )
--
     R=CC[x,u1]
     f1=x^2-u1
     finalParameters0={1}
     finalParameters1={2}
     dir1 := temporaryFileName(); -- build a directory to store temporary data 
     makeDirectory dir1;  
     bPH5=bertiniParameterHomotopy( {f1}, {u1},{finalParameters0 ,finalParameters1 },AffVariableGroup=>{x},OutputStyle=>"OutNone",TopDirectory=>dir1)            
     B0=importSolutionsFile(dir1,NameSolutionsFile=>"ph_jade_0")     
     B1=importSolutionsFile(dir1,NameSolutionsFile=>"ph_jade_1")     
--TODO: need new assertions
--      assert(1===#radicalList({ (B0_0_0)^2,1})     )
--      assert(1===#radicalList({ (B1_0_0)^2,2})     )






