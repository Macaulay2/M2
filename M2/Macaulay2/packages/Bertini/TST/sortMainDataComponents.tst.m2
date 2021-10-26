needsPackage "Bertini"

--   Example
     F={"x*(x+2*y+3*z^2)","(y^3-x+z)*(z)*(x+2*y+3*z^2)"}
     makeB'InputFile(storeBM2Files,B'Configs=>{{TrackType,1}},AffVariableGroup=>{x,y,z},B'Polynomials=>F)
     runBertini(storeBM2Files)
     listPoints=importMainDataFile(storeBM2Files)
     assert(#listPoints==6)
     theComponents=sortMainDataComponents(listPoints)
     c0=for i in theComponents_0 list (i#ComponentNumber,i#Dimension)     
     c1=for i in theComponents_1 list (i#ComponentNumber,i#Dimension)          
     c2=for i in theComponents_2 list (i#ComponentNumber,i#Dimension)          
     for oneC in  {c0,c1,c2} do if  #oneC==1 then assert(oneC=={(0, 1)})
     for oneC in  {c0,c1,c2} do if #oneC==2 then assert(oneC=={(0, 2), (0, 2)})
     for oneC in  {c0,c1,c2} do if #oneC==3 then assert(oneC=={(1, 1), (1, 1), (1, 1)})
     

     