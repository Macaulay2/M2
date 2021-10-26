needsPackage "Bertini"

     R=CC[x,y,z]
     f=z*x+y
     e1=subPoint(f,{x,y},{.1,.2})
     assert(size e1==2)
     e2=subPoint(f,{x,y,z},{.1,.2,.3},SpecifyVariables=>{y})
     assert(size e2==2)
--
     R=CC_200[x,y,z]
     f=z*x+y
     e3=subPoint(f,{x,y,z},{.1,.2,.3},SubIntoCC=>true)
     assert(CC===    class e3)
     e4=toExternalString subPoint(f,{x,y,z},{.1234567890123456789012345678901234567890p200,
	     0,1},SubIntoCC=>true,M2Precision=>200)
          assert(length e4>75)
