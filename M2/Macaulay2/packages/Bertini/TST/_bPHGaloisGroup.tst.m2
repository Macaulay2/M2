needsPackage "Bertini"


     R=CC[x,T]
     f=x^6+2*x^4+3*x^2+T
     makeB'InputFile(storeBM2Files,AffVariableGroup=>{x,T},B'Polynomials=>{f,diff(x,f)})
     runBertini(storeBM2Files)
     TCoordinates=importSolutionsFile(storeBM2Files)/last
     TBranchPoints=radicalList(TCoordinates)
     makeB'InputFile(storeBM2Files,B'Configs=>{{ParameterHomotopy,1}},AffVariableGroup=>{x},ParameterGroup=>{T},B'Polynomials=>{f})
     runBertini(storeBM2Files,PreparePH2=>true)
     gg1=b'PHGaloisGroup(storeBM2Files,BranchPoints=>TBranchPoints)
     assert(#gg1===3)
     assert member(false,for i in gg1 list i===[1,2,3,4,5,6])--says we find a nontrivial generator.
     assert member(false,for i in drop(gg1,1) list i===gg1_0)--says the generators are not all the same.

     gg2=b'PHGaloisGroup(storeBM2Files,BranchPoints=>TBranchPoints,LoopRadius=>.5)
     assert(#gg2===3)
     assert member(false,for i in gg2 list i===[1,2,3,4,5,6])--says we find a nontrivial generator.
     assert member(false,for i in drop(gg2,1) list i===gg2_0)--says the generators are not all the same.

     gg3=b'PHGaloisGroup(storeBM2Files,BranchPoints=>{TBranchPoints_0})      
     assert(#gg3===1)
---StorageFolder
     R=CC[x,T]
     f=x^6+2*x^4+3*x^2+T
     makeB'InputFile(storeBM2Files,AffVariableGroup=>{x,T},B'Polynomials=>{f,diff(x,f)})
     runBertini(storeBM2Files)
     TCoordinates=importSolutionsFile(storeBM2Files)/last
     TBranchPoints=radicalList(TCoordinates)
     makeB'InputFile(storeBM2Files,B'Configs=>{{ParameterHomotopy,1}},AffVariableGroup=>{x},ParameterGroup=>{T},B'Polynomials=>{f})
     runBertini(storeBM2Files,PreparePH2=>true)
     gg4=b'PHGaloisGroup(storeBM2Files,BranchPoints=>TBranchPoints,StorageFolder=>"StoreFiles")
     assert(#gg4===3)
     assert member(false,for i in gg4 list i===[1,2,3,4,5,6])--says we find a nontrivial generator.
     assert member(false,for i in drop(gg4,1) list i===gg4_0)--says the generators are not all the same.

     gg5=b'PHGaloisGroup(storeBM2Files,BranchPoints=>TBranchPoints,LoopRadius=>.5,StorageFolder=>"StoreFiles")
     assert(#gg5===3)
     assert member(false,for i in gg5 list i===[1,2,3,4,5,6])--says we find a nontrivial generator.
     assert member(false,for i in drop(gg5,1) list i===gg5_0)--says the generators are not all the same.

     gg6=b'PHGaloisGroup(storeBM2Files,BranchPoints=>{TBranchPoints_0},StorageFolder=>"StoreFiles")      
     assert(#gg6===1)
